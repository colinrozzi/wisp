; Label Store Actor - Gossip-about-Gossip DAG Consensus
;
; A distributed label system using a DAG of events for consensus.
; Each event references its parent events, creating a graph that
; encodes "who knew what when" - enabling decentralized finality.
;
; Event structure:
;   - label: The label being set
;   - content_hash: The content hash it points to
;   - parents: List of parent event hashes
;   - author: Public key of creator

; ============================================================
; Imports
; ============================================================

(import theater:simple/runtime log ((msg string)) s32)
(import theater:simple/store new () (result string string))
(import theater:simple/store store
  ((store-id string) (content (list u8)))
  (result string string))
(import theater:simple/store get
  ((store-id string) (content-ref string))
  (result (list u8) string))
(import theater:simple/store label
  ((store-id string) (lbl string) (content-ref string))
  (result s32 string))
(import theater:simple/store get-by-label
  ((store-id string) (lbl string))
  (result (option string) string))

; ============================================================
; Data Types
; ============================================================

; A hash is 32 bytes, represented as a hex string for simplicity

; An event in the DAG
(record event
  (lbl string)
  (content-hash string)
  (parents (list string))
  (author string))

; Association list entry for labels -> content hash
(record label-entry
  (name string)
  (hash string))

; Association list entry for event hash -> event data
(record event-entry
  (hash string)
  (evt event))

; The actor state
(record actor-state
  (store-id string)
  (author string)
  (events (list event-entry))
  (frontier (list string))
  (labels (list label-entry))
  (participants (list string))
  (finalized (list string)))

; ============================================================
; List Utilities
; ============================================================

; Get tail of a list (skip first n elements)
(fn list-skip ((lst (list string)) (n s32)) (list string)
  (if (i32.le_s n 0)
    lst
    (if (i32.eq (list-len lst) 0)
      lst
      (list-skip-acc lst n 0 (list-new string)))))

(fn list-skip-acc ((lst (list string)) (skip s32) (idx s32) (acc (list string))) (list string)
  (if (i32.ge_s idx (list-len lst))
    acc
    (if (i32.lt_s idx skip)
      (list-skip-acc lst skip (i32.add idx 1) acc)
      (list-skip-acc lst skip (i32.add idx 1) (list-push acc (list-get lst idx))))))

; Skip for label-entry lists
(fn label-list-skip ((lst (list label-entry)) (n s32)) (list label-entry)
  (if (i32.le_s n 0)
    lst
    (if (i32.eq (list-len lst) 0)
      lst
      (label-list-skip-acc lst n 0 (list-new label-entry)))))

(fn label-list-skip-acc ((lst (list label-entry)) (skip s32) (idx s32) (acc (list label-entry))) (list label-entry)
  (if (i32.ge_s idx (list-len lst))
    acc
    (if (i32.lt_s idx skip)
      (label-list-skip-acc lst skip (i32.add idx 1) acc)
      (label-list-skip-acc lst skip (i32.add idx 1) (list-push acc (list-get lst idx))))))

; Skip for event-entry lists
(fn event-list-skip ((lst (list event-entry)) (n s32)) (list event-entry)
  (if (i32.le_s n 0)
    lst
    (if (i32.eq (list-len lst) 0)
      lst
      (event-list-skip-acc lst n 0 (list-new event-entry)))))

(fn event-list-skip-acc ((lst (list event-entry)) (skip s32) (idx s32) (acc (list event-entry))) (list event-entry)
  (if (i32.ge_s idx (list-len lst))
    acc
    (if (i32.lt_s idx skip)
      (event-list-skip-acc lst skip (i32.add idx 1) acc)
      (event-list-skip-acc lst skip (i32.add idx 1) (list-push acc (list-get lst idx))))))

; ============================================================
; Set Operations (using lists)
; ============================================================

; Check if string is in list
(fn member ((lst (list string)) (elem string)) s32
  (if (i32.eq (list-len lst) 0)
    0
    (if (string=? (list-get lst 0) elem)
      1
      (member (list-skip lst 1) elem))))

; Add to set if not present
(fn set-add ((lst (list string)) (elem string)) (list string)
  (if (member lst elem)
    lst
    (list-push lst elem)))

; Remove from set
(fn set-remove ((lst (list string)) (elem string)) (list string)
  (set-remove-acc lst elem (list-new string)))

(fn set-remove-acc ((lst (list string)) (elem string) (acc (list string))) (list string)
  (if (i32.eq (list-len lst) 0)
    acc
    (let (head (list-get lst 0))
      (let (rest (list-skip lst 1))
        (if (string=? head elem)
          (set-remove-acc rest elem acc)
          (set-remove-acc rest elem (list-push acc head)))))))

; ============================================================
; Association List Operations
; ============================================================

; Look up a key in a label association list
(fn label-lookup ((lst (list label-entry)) (key string)) (option string)
  (if (i32.eq (list-len lst) 0)
    (none string)
    (let (entry (list-get lst 0))
      (if (string=? (label-entry.name entry) key)
        (some string (label-entry.hash entry))
        (label-lookup (label-list-skip lst 1) key)))))

; Insert or update in label association list
(fn label-insert ((lst (list label-entry)) (key string) (val string)) (list label-entry)
  (list-push (label-remove lst key) (label-entry key val)))

; Remove from label association list
(fn label-remove ((lst (list label-entry)) (key string)) (list label-entry)
  (label-remove-acc lst key (list-new label-entry)))

(fn label-remove-acc ((lst (list label-entry)) (key string) (acc (list label-entry))) (list label-entry)
  (if (i32.eq (list-len lst) 0)
    acc
    (let (entry (list-get lst 0))
      (let (rest (label-list-skip lst 1))
        (if (string=? (label-entry.name entry) key)
          (label-remove-acc rest key acc)
          (label-remove-acc rest key (list-push acc entry)))))))

; ============================================================
; Event Operations
; ============================================================

; Look up an event by hash
(fn event-lookup ((events (list event-entry)) (hash string)) (option event)
  (if (i32.eq (list-len events) 0)
    (none event)
    (let (entry (list-get events 0))
      (if (string=? (event-entry.hash entry) hash)
        (some event (event-entry.evt entry))
        (event-lookup (event-list-skip events 1) hash)))))

; Add an event to the list
(fn event-add ((events (list event-entry)) (hash string) (evt event)) (list event-entry)
  (list-push events (event-entry hash evt)))

; ============================================================
; DAG Traversal - Can event A "see" event B?
; ============================================================

; Check if 'from' can see 'to' by traversing parents
(fn can-see ((state actor-state) (from string) (to string)) s32
  (if (string=? from to)
    1
    (can-see-search state (list-push (list-new string) from) (list-new string) to)))

; BFS search through parents
(fn can-see-search ((state actor-state) (queue (list string)) (visited (list string)) (target string)) s32
  (if (i32.eq (list-len queue) 0)
    0
    (let (current (list-get queue 0))
      (let (rest-queue (list-skip queue 1))
        (if (string=? current target)
          1
          (if (member visited current)
            (can-see-search state rest-queue visited target)
            (let (new-visited (set-add visited current))
              (match (event-lookup (actor-state.events state) current)
                ((some evt)
                  (can-see-search state
                    (add-parents-to-queue rest-queue (event.parents evt))
                    new-visited
                    target))
                ((none)
                  (can-see-search state rest-queue new-visited target))))))))))

; Add parent hashes to the queue
(fn add-parents-to-queue ((queue (list string)) (parents (list string))) (list string)
  (if (i32.eq (list-len parents) 0)
    queue
    (add-parents-to-queue
      (list-push queue (list-get parents 0))
      (list-skip parents 1))))

; ============================================================
; Finality - Supermajority Detection
; ============================================================

; Count how many unique participants can see an event
(fn count-seers ((state actor-state) (event-hash string)) s32
  (count-seers-acc state event-hash (actor-state.events state) (list-new string)))

(fn count-seers-acc ((state actor-state) (target string) (events (list event-entry)) (seers (list string))) s32
  (if (i32.eq (list-len events) 0)
    (list-len seers)
    (let (entry (list-get events 0))
      (let (rest (event-list-skip events 1))
        (let (evt (event-entry.evt entry))
          (let (evt-hash (event-entry.hash entry))
            (if (can-see state evt-hash target)
              (count-seers-acc state target rest (set-add seers (event.author evt)))
              (count-seers-acc state target rest seers))))))))

; Check if an event is "strongly seen" (seen by 2/3+ of participants)
(fn is-strongly-seen ((state actor-state) (event-hash string)) s32
  (let (n (list-len (actor-state.participants state)))
    (if (i32.eq n 0)
      0
      (let (threshold (i32.div_s (i32.add (i32.mul n 2) 2) 3))
        (let (seers (count-seers state event-hash))
          (i32.ge_s seers threshold))))))

; Check all non-finalized events for finality
(fn check-finality ((state actor-state)) actor-state
  (check-finality-acc state (actor-state.events state)))

(fn check-finality-acc ((state actor-state) (events (list event-entry))) actor-state
  (if (i32.eq (list-len events) 0)
    state
    (let (entry (list-get events 0))
      (let (rest (event-list-skip events 1))
        (let (hash (event-entry.hash entry))
          (if (member (actor-state.finalized state) hash)
            (check-finality-acc state rest)
            (if (is-strongly-seen state hash)
              (let (_ (log (string-append "Event finalized: " (substring hash 0 16))))
                (check-finality-acc
                  (actor-state
                    (actor-state.store-id state)
                    (actor-state.author state)
                    (actor-state.events state)
                    (actor-state.frontier state)
                    (actor-state.labels state)
                    (actor-state.participants state)
                    (set-add (actor-state.finalized state) hash))
                  rest))
              (check-finality-acc state rest))))))))

; ============================================================
; Event Creation
; ============================================================

; Create a new event with current frontier as parents
(fn create-event ((state actor-state) (label-name string) (content-hash string)) event
  (event
    label-name
    content-hash
    (actor-state.frontier state)
    (actor-state.author state)))

; Add an event to state and update frontier
(fn add-event-to-state ((state actor-state) (evt event) (evt-hash string)) actor-state
  (let (new-events (event-add (actor-state.events state) evt-hash evt))
    (let (new-frontier (update-frontier (actor-state.frontier state) (event.parents evt) evt-hash))
      (let (new-labels (label-insert (actor-state.labels state) (event.lbl evt) (event.content-hash evt)))
        (check-finality
          (actor-state
            (actor-state.store-id state)
            (actor-state.author state)
            new-events
            new-frontier
            new-labels
            (actor-state.participants state)
            (actor-state.finalized state)))))))

; Update frontier: remove parents, add new event
(fn update-frontier ((frontier (list string)) (parents (list string)) (new-hash string)) (list string)
  (set-add (remove-all frontier parents) new-hash))

; Remove all elements in 'to-remove' from 'lst'
(fn remove-all ((lst (list string)) (to-remove (list string))) (list string)
  (if (i32.eq (list-len to-remove) 0)
    lst
    (remove-all
      (set-remove lst (list-get to-remove 0))
      (list-skip to-remove 1))))

; ============================================================
; Hash Computation (placeholder - needs SHA-256)
; ============================================================

; TODO: Implement SHA-256 or import from host
; For now, we use a simple concatenation as pseudo-hash
(fn compute-event-hash ((evt event)) string
  (string-append "evt-"
    (string-append (event.lbl evt)
      (string-append "-" (event.content-hash evt)))))

; ============================================================
; Actor Handlers
; ============================================================

; Initialize the actor
(fn init ((state (option (list u8))))
    (result (tuple (option (list u8))) string)
  (let (_ (log "Label store (gossip) starting..."))
    (match state
      ((some bytes)
        (ok (tuple (option (list u8))) string (tuple (some (list u8) bytes))))
      ((none)
        (match (new)
          ((ok store-id)
            (let (author (string-append "node-" (substring store-id 0 8)))
              (let (_ (log (string-append "Created store: " store-id)))
                (let (initial-state (actor-state
                        store-id
                        author
                        (list-new event-entry)
                        (list-new string)
                        (list-new label-entry)
                        (list-push (list-new string) author)
                        (list-new string)))
                  (ok (tuple (option (list u8))) string (tuple (some (list u8) (list-new u8))))))))
          ((err e) (err (tuple (option (list u8))) string e)))))))

; Set a label - creates a new event in the DAG
; For now, simplified to just take state bytes and return event hash
(fn set-label ((state-bytes (list u8)) (label-name string) (content-hash string))
    (result (tuple (list u8) string) string)
  (let (_ (log (string-append "Setting label: " label-name)))
    ; TODO: deserialize state, create event, serialize state
    ; For now, return placeholder
    (ok (tuple (list u8) string) string (tuple state-bytes "placeholder-hash"))))

; Get the content hash for a label
(fn get-label ((state-bytes (list u8)) (label-name string))
    (result (tuple (list u8) (option string)) string)
  ; TODO: deserialize state and lookup
  (ok (tuple (list u8) (option string)) string (tuple state-bytes (none string))))

; Get count of events in DAG
(fn events-count ((state-bytes (list u8)))
    (result (tuple (list u8) s32) string)
  ; TODO: deserialize state
  (ok (tuple (list u8) s32) string (tuple state-bytes 0)))

; Get count of finalized events
(fn finalized-count ((state-bytes (list u8)))
    (result (tuple (list u8) s32) string)
  ; TODO: deserialize state
  (ok (tuple (list u8) s32) string (tuple state-bytes 0)))

; ============================================================
; Exports
; ============================================================

(export init)
(export set-label)
(export get-label)
(export events-count)
(export finalized-count)
