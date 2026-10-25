//! The boot list: services the daemon starts at `serve` and restarts when they
//! die. A service is a session with a fixed name whose start file (a Wisp
//! source under the filesystem root) is evaluated in it, the way
//! `theater-repl new web && theater-repl eval web - < serve.wisp` would.
//!
//! This is the userland supervisor of the supervision design (theater#235,
//! `changes/proposals/2026-10-24-supervision.md` §2.3): the runtime neither
//! restarts nor owns anything. The daemon hears every death from its global
//! chain subscription ([`wisp_interpreter_actor::deaths`]), and a restart is a
//! fresh session (a new actor id) with the same name, running the start file
//! again. Policies and the intensity limit follow OTP: `permanent` always
//! restarts, `transient` only after an abnormal death, `temporary` never; more
//! than `max_restarts` restarts within `period_secs` and the daemon gives up.
//!
//! A start file that fails counts as a death: the daemon stops that session
//! and applies the policy. `theater-repl stop` on a service's session marks it
//! stopped first, so its death is not restarted; `theater-repl start NAME`
//! starts it again with a fresh limit.
use crate::Daemon;
use anyhow::{Context, Result};
use std::collections::VecDeque;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};
use theater::TheaterId;
use theater::events::lifecycle::TerminationCause;
use wisp_interpreter_actor::{deaths, sessions::Note, short_id};

/// When a service's death is followed by a restart.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Restart {
    /// Always.
    #[default]
    Permanent,
    /// Unless it ended normally: completed, or stopped.
    Transient,
    /// Never.
    Temporary,
}

impl Restart {
    fn name(self) -> &'static str {
        match self {
            Restart::Permanent => "permanent",
            Restart::Transient => "transient",
            Restart::Temporary => "temporary",
        }
    }
}

/// One `[[service]]` table of the config.
#[derive(Debug, Clone, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ServiceConfig {
    /// The session's name, and the service's.
    pub name: String,
    /// The Wisp file evaluated in each fresh session, relative to the
    /// filesystem root. Read again at every start, so edits take effect.
    pub start: PathBuf,
    #[serde(default)]
    pub restart: Restart,
    /// At most this many restarts within `period_secs`; one more death and the
    /// daemon gives up.
    #[serde(default = "default_max_restarts")]
    pub max_restarts: u32,
    #[serde(default = "default_period_secs")]
    pub period_secs: u64,
}

fn default_max_restarts() -> u32 {
    5
}

fn default_period_secs() -> u64 {
    60
}

/// Check a boot list against the filesystem root its start files live under.
pub fn validate(services: &[ServiceConfig], root: Option<&Path>) -> Result<()> {
    if !services.is_empty() && root.is_none() {
        anyhow::bail!("[[service]] needs a [filesystem] root to find its start file");
    }
    for (i, service) in services.iter().enumerate() {
        let name = &service.name;
        anyhow::ensure!(
            !name.is_empty() && !name.contains(char::is_whitespace),
            "service name {name:?} must be one word"
        );
        anyhow::ensure!(
            !services[..i].iter().any(|other| other.name == *name),
            "two services are named {name}"
        );
        anyhow::ensure!(
            service
                .start
                .components()
                .all(|part| matches!(part, Component::Normal(_)))
                && service.start.components().next().is_some(),
            "service {name}: start {} must be a path under the filesystem root",
            service.start.display()
        );
    }
    Ok(())
}

/// Why a service's session ended.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Death {
    pub text: String,
    /// Completed or stopped: what `transient` does not restart.
    pub normal: bool,
}

impl Death {
    fn of(cause: &TerminationCause) -> Self {
        Self {
            text: deaths::describe(cause),
            normal: matches!(
                cause,
                TerminationCause::Completed { .. } | TerminationCause::Stopped
            ),
        }
    }

    fn start_failed(error: &str) -> Self {
        Self {
            text: format!("start file failed: {error}"),
            normal: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum State {
    /// Its first start is under way.
    Starting,
    Running,
    /// Starting again after a death.
    Restarting,
    /// By `theater-repl stop`.
    Stopped,
    /// Its policy does not restart this death.
    Exited,
    /// Past the intensity limit.
    GaveUp,
}

impl State {
    fn name(self) -> &'static str {
        match self {
            State::Starting => "starting",
            State::Running => "running",
            State::Restarting => "restarting",
            State::Stopped => "stopped",
            State::Exited => "exited",
            State::GaveUp => "given up",
        }
    }

    /// A start is under way: its task owns the service until it finishes.
    fn launching(self) -> bool {
        matches!(self, State::Starting | State::Restarting)
    }
}

/// What a death leads to.
#[derive(Debug, PartialEq, Eq)]
pub enum Decision {
    Restart,
    Exit,
    GiveUp,
}

pub struct Service {
    pub spec: ServiceConfig,
    pub state: State,
    /// The session's actor, while there is one.
    pub actor: Option<TheaterId>,
    /// Restarts since boot or since the last `start`.
    pub restarts: u32,
    /// When the restarts within the period happened, oldest first.
    recent: VecDeque<Instant>,
    pub last_death: Option<(Death, Instant)>,
    /// A death noticed while a start was under way, for that start to handle.
    pending: Option<Death>,
    /// Bumped by `stop` and `start`, so a start task that is overtaken quits.
    generation: u64,
}

impl Service {
    pub fn new(spec: ServiceConfig) -> Self {
        Self {
            spec,
            state: State::Starting,
            actor: None,
            restarts: 0,
            recent: VecDeque::new(),
            last_death: None,
            pending: None,
            generation: 0,
        }
    }

    /// Record a death at `now` and decide what follows. A restart is counted
    /// here, against the limit.
    pub fn died(&mut self, death: Death, now: Instant) -> Decision {
        self.actor = None;
        self.pending = None;
        let restart = match self.spec.restart {
            Restart::Permanent => true,
            Restart::Transient => !death.normal,
            Restart::Temporary => false,
        };
        self.last_death = Some((death, now));
        if !restart {
            self.state = State::Exited;
            return Decision::Exit;
        }
        let period = Duration::from_secs(self.spec.period_secs);
        while self
            .recent
            .front()
            .is_some_and(|at| now.duration_since(*at) >= period)
        {
            self.recent.pop_front();
        }
        if self.recent.len() >= self.spec.max_restarts as usize {
            self.state = State::GaveUp;
            return Decision::GiveUp;
        }
        self.recent.push_back(now);
        self.restarts += 1;
        self.state = State::Restarting;
        Decision::Restart
    }
}

/// The boot list and each service's state.
pub struct Services {
    root: PathBuf,
    table: std::sync::Mutex<Vec<Service>>,
    /// Set as the daemon goes down: deaths then are not restarted.
    closing: AtomicBool,
}

impl Services {
    pub fn new(specs: Vec<ServiceConfig>, root: Option<PathBuf>) -> Self {
        Self {
            root: root.unwrap_or_default(),
            table: std::sync::Mutex::new(specs.into_iter().map(Service::new).collect()),
            closing: AtomicBool::new(false),
        }
    }

    fn table(&self) -> std::sync::MutexGuard<'_, Vec<Service>> {
        self.table.lock().unwrap_or_else(|p| p.into_inner())
    }

    fn with<T>(&self, name: &str, f: impl FnOnce(&mut Service) -> T) -> Option<T> {
        self.table().iter_mut().find(|s| s.spec.name == name).map(f)
    }

    pub fn is_empty(&self) -> bool {
        self.table().is_empty()
    }

    pub fn close(&self) {
        self.closing.store(true, Ordering::SeqCst);
    }

    fn closing(&self) -> bool {
        self.closing.load(Ordering::SeqCst)
    }

    /// `theater-repl stop` is about to stop actor `id`: if it is a service's,
    /// mark the service stopped so its death is not restarted.
    pub fn stopping(&self, id: TheaterId) {
        for service in self.table().iter_mut() {
            if service.actor == Some(id) {
                log(&service.spec.name, "stopped by request; not restarting");
                service.actor = None;
                service.state = State::Stopped;
                service.generation += 1;
            }
        }
    }

    /// `theater-repl stop NAME` for a service with no session to stop (it is
    /// between restarts, or done): mark it stopped. False if there's none.
    pub fn stop_named(&self, name: &str) -> bool {
        self.with(name, |service| {
            if service.state != State::Stopped {
                log(name, "stopped by request; not restarting");
            }
            service.state = State::Stopped;
            service.generation += 1;
        })
        .is_some()
    }

    /// The table `status` and `services` print.
    pub fn render(&self, interpreters: &[wisp_interpreter_actor::Interpreter]) -> String {
        let table = self.table();
        if table.is_empty() {
            return "no services (a [[service]] table in the config adds one)".into();
        }
        let mut out = format!(
            "{:<16} {:<10} {:>8}  {:<9} {}",
            "NAME", "STATE", "RESTARTS", "ID", "LAST DEATH"
        );
        for service in table.iter() {
            let id = service
                .actor
                .map_or("-".into(), |id| short_id(interpreters, &id));
            let death = match &service.last_death {
                Some((death, at)) => format!("{} ({}s ago)", death.text, at.elapsed().as_secs()),
                None => "-".into(),
            };
            out.push_str(&format!(
                "\n{:<16} {:<10} {:>8}  {:<9} {death}",
                service.spec.name,
                service.state.name(),
                service.restarts,
                id,
            ));
        }
        out
    }
}

fn log(name: &str, what: &str) {
    eprintln!("service {name}: {what}");
}

/// Start every service, then supervise: one task hears every death.
pub fn boot(daemon: &Arc<Daemon>) {
    let deaths = daemon
        .runtime
        .deaths
        .lock()
        .unwrap_or_else(|p| p.into_inner())
        .take();
    let Some(mut deaths) = deaths else {
        return;
    };
    let names: Vec<String> = daemon
        .services
        .table()
        .iter()
        .map(|s| s.spec.name.clone())
        .collect();
    for name in names {
        tokio::spawn(launch(daemon.clone(), name, 0));
    }
    let daemon = daemon.clone();
    tokio::spawn(async move {
        while let Some((id, cause)) = deaths.recv().await {
            if daemon.services.closing() {
                continue;
            }
            let death = Death::of(&cause);
            let found = daemon.services.table().iter_mut().find_map(|service| {
                if service.actor != Some(id) {
                    return None;
                }
                if service.state.launching() {
                    // Its start task is waiting on this actor and will fail;
                    // it handles the death.
                    service.pending = Some(death.clone());
                    return None;
                }
                Some(service.spec.name.clone())
            });
            if let Some(name) = found {
                died(&daemon, &name, death);
            }
        }
    });
}

/// `theater-repl start NAME`: start a service that is not running, with a
/// fresh limit.
pub fn start(daemon: &Arc<Daemon>, name: &str) -> Result<String> {
    let generation = daemon
        .services
        .with(name, |service| {
            if service.state.launching() || service.state == State::Running {
                anyhow::bail!("service {name} is {}", service.state.name());
            }
            service.state = State::Starting;
            service.recent.clear();
            service.restarts = 0;
            service.generation += 1;
            Ok(service.generation)
        })
        .with_context(|| format!("no service named {name}"))??;
    log(name, "starting by request");
    tokio::spawn(launch(daemon.clone(), name.to_string(), generation));
    Ok(format!("starting service {name}"))
}

/// Apply the policy to a death of service `name`, and restart it if that is
/// the decision.
fn died(daemon: &Arc<Daemon>, name: &str, death: Death) {
    let text = death.text.clone();
    let Some((decision, generation, spec)) = daemon.services.with(name, |service| {
        let decision = service.died(death, Instant::now());
        (decision, service.generation, service.spec.clone())
    }) else {
        return;
    };
    match decision {
        Decision::Restart => {
            log(name, &format!("died ({text}); restarting"));
            tokio::spawn(launch(daemon.clone(), name.to_string(), generation));
        }
        Decision::Exit => log(
            name,
            &format!(
                "ended ({text}); {} does not restart it",
                spec.restart.name()
            ),
        ),
        Decision::GiveUp => log(
            name,
            &format!(
                "died ({text}); giving up: more than {} restarts in {}s",
                spec.max_restarts, spec.period_secs
            ),
        ),
    }
}

/// Start service `name`: a fresh session under its name, running its start
/// file. Quits quietly if the service is stopped or started again meanwhile.
async fn launch(daemon: Arc<Daemon>, name: String, generation: u64) {
    match try_launch(&daemon, &name, generation).await {
        Ok(Some(id)) => log(&name, &format!("running as {id}")),
        Ok(None) => {}
        Err(error) => {
            let current = daemon
                .services
                .with(&name, |service| {
                    (service.generation == generation).then(|| {
                        service.actor = None;
                        service.pending.take()
                    })
                })
                .flatten();
            if let Some(pending) = current
                && !daemon.services.closing()
            {
                let error = match pending {
                    Some(death) => format!("{error:#} ({})", death.text),
                    None => format!("{error:#}"),
                };
                died(&daemon, &name, Death::start_failed(&error));
            }
        }
    }
}

/// Is this start still wanted?
fn current(daemon: &Daemon, name: &str, generation: u64) -> bool {
    !daemon.services.closing()
        && daemon
            .services
            .with(name, |s| s.generation == generation)
            .unwrap_or(false)
}

/// The steps of a start. Ok(None) when it was overtaken; an error is a failed
/// start, and the session it made (if any) is stopped.
async fn try_launch(
    daemon: &Arc<Daemon>,
    name: &str,
    generation: u64,
) -> Result<Option<TheaterId>> {
    let Some(start) = daemon.services.with(name, |s| s.spec.start.clone()) else {
        return Ok(None);
    };
    // The dead session leaves the runtime's list a little after its death.
    // Wait for it, so the name is not shared.
    let deadline = Instant::now() + Duration::from_secs(15);
    loop {
        let taken = daemon
            .runtime
            .interpreters()
            .await?
            .into_iter()
            .find(|interpreter| interpreter.name == name);
        match taken {
            None => break,
            Some(other) if Instant::now() >= deadline => {
                anyhow::bail!("a session named {name} already exists: {}", other.id)
            }
            Some(_) => tokio::time::sleep(Duration::from_millis(100)).await,
        }
    }
    let path = daemon.services.root.join(&start);
    let source =
        std::fs::read_to_string(&path).with_context(|| format!("reading {}", path.display()))?;
    if !current(daemon, name, generation) {
        return Ok(None);
    }
    let actor = daemon.default.lock().await.clone();
    let session = daemon
        .runtime
        .spawn_named((*actor.wasm).clone(), actor.manifest.clone(), name.into())
        .await
        .context("creating the session")?;
    daemon.runtime.sessions.note(
        session.id,
        Note {
            source: actor.origin,
            since: Instant::now(),
        },
    );
    let owned = daemon
        .services
        .with(name, |s| {
            let owned = s.generation == generation && !daemon.services.closing();
            if owned {
                s.actor = Some(session.id);
            }
            owned
        })
        .unwrap_or(false);
    if !owned {
        let _ = session.stop().await;
        return Ok(None);
    }
    let result = match session.evaluate(&source).await {
        Ok(output) if output.starts_with("error:") => Err(anyhow::anyhow!("{output}")),
        Ok(_) => Ok(()),
        Err(error) => Err(error),
    };
    if let Err(error) = result {
        // Forget the actor before stopping it, so its death is not a second one.
        daemon.services.with(name, |s| s.actor = None);
        let _ = session.stop().await;
        return Err(error.context(start.display().to_string()));
    }
    // The start is done; a death noticed during it is handled now.
    let pending = daemon.services.with(name, |s| {
        if s.generation != generation || !s.state.launching() {
            return None;
        }
        match s.pending.take() {
            Some(death) => Some(Some(death)),
            None => {
                s.state = State::Running;
                Some(None)
            }
        }
    });
    match pending.flatten() {
        Some(Some(death)) => {
            died(daemon, name, death);
            Ok(None)
        }
        Some(None) => Ok(Some(session.id)),
        None => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn spec(restart: Restart, max_restarts: u32, period_secs: u64) -> ServiceConfig {
        ServiceConfig {
            name: "web".into(),
            start: "serve.wisp".into(),
            restart,
            max_restarts,
            period_secs,
        }
    }

    fn death(normal: bool) -> Death {
        Death {
            text: "x".into(),
            normal,
        }
    }

    #[test]
    fn permanent_restarts_until_the_limit() {
        let mut s = Service::new(spec(Restart::Permanent, 2, 10));
        let t = Instant::now();
        assert_eq!(s.died(death(true), t), Decision::Restart);
        assert_eq!(
            s.died(death(false), t + Duration::from_secs(1)),
            Decision::Restart
        );
        assert_eq!(
            s.died(death(false), t + Duration::from_secs(2)),
            Decision::GiveUp
        );
        assert_eq!(s.state, State::GaveUp);
        assert_eq!(s.restarts, 2);
    }

    #[test]
    fn restarts_outside_the_period_do_not_count() {
        let mut s = Service::new(spec(Restart::Permanent, 1, 10));
        let t = Instant::now();
        assert_eq!(s.died(death(false), t), Decision::Restart);
        assert_eq!(
            s.died(death(false), t + Duration::from_secs(10)),
            Decision::Restart
        );
        assert_eq!(
            s.died(death(false), t + Duration::from_secs(11)),
            Decision::GiveUp
        );
    }

    #[test]
    fn transient_and_temporary() {
        let t = Instant::now();
        let mut s = Service::new(spec(Restart::Transient, 5, 10));
        assert_eq!(s.died(death(false), t), Decision::Restart);
        assert_eq!(s.died(death(true), t), Decision::Exit);
        assert_eq!(s.state, State::Exited);
        let mut s = Service::new(spec(Restart::Temporary, 5, 10));
        assert_eq!(s.died(death(false), t), Decision::Exit);
        assert_eq!(s.restarts, 0);
    }

    #[test]
    fn zero_restarts_gives_up_at_the_first_death() {
        let mut s = Service::new(spec(Restart::Permanent, 0, 10));
        assert_eq!(s.died(death(false), Instant::now()), Decision::GiveUp);
    }

    #[test]
    fn config_defaults_and_validation() {
        let c: ServiceConfig = toml::from_str("name = \"web\"\nstart = \"serve.wisp\"").unwrap();
        assert_eq!(c.restart, Restart::Permanent);
        assert_eq!((c.max_restarts, c.period_secs), (5, 60));
        let root = Path::new("/srv");
        assert!(validate(std::slice::from_ref(&c), Some(root)).is_ok());
        assert!(validate(std::slice::from_ref(&c), None).is_err());
        assert!(validate(&[c.clone(), c.clone()], Some(root)).is_err());
        for bad in ["/etc/x.wisp", "../x.wisp", "a/../../x.wisp", ""] {
            let mut c = c.clone();
            c.start = bad.into();
            assert!(validate(&[c], Some(root)).is_err(), "{bad}");
        }
        assert!(
            toml::from_str::<ServiceConfig>("name = \"w\"\nstart = \"s\"\nrestart = \"sometimes\"")
                .is_err()
        );
    }
}
