(define-module (i3 sync)
  #:use-module (i3 core)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 q)
  #:export (;; Connection
            i3-ipc-sync?
            make-i3-ipc-sync
            ;; Functions
            i3-do
            i3-next-event))

(define-record-type <i3-ipc-sync>
  (make-i3-ipc-sync-internal connection event-queue)
  i3-ipc-sync?
  ;; Connection
  (connection i3-ipc-connection)
  ;; Events
  (event-queue i3-ipc-event-queue))

;;;; Internals

(define (get-reply i3-ipc type)
  (let get-reply-loop ()
    (let ((reply (i3-receive-reply (i3-ipc-connection i3-ipc))))
      (cond
       ((i3-event? reply)
        (enq! (i3-ipc-event-queue i3-ipc) reply)
        (get-reply-loop))
       ((i3-reply? reply)
        (unless (= type (i3-reply-type reply))
          (error "Unexpected reply from i3"))
        (i3-reply-body reply))
       (else
        (error "Unsupported reply type"))))))

(define (get-event i3-ipc)
  (let ((event-queue (i3-ipc-event-queue i3-ipc)))
    (if (not (q-empty? event-queue))
        (deq! event-queue)
        (let ((reply (i3-receive-reply (i3-ipc-connection i3-ipc))))
          (cond
           ((i3-event? reply)
            reply)
           ((i3-reply? reply)
            (error "Unexpected reply from i3"))
           (else
            (error "Unsupported reply type")))))))

;;;; Mid level

(define (make-i3-ipc-sync)
  (make-i3-ipc-sync-internal
   (connect-to-i3)
   (make-q)))

(define (i3-do i3-ipc type body)
  (i3-send-message! (i3-ipc-connection i3-ipc) type body)
  (get-reply i3-ipc type))

(define (i3-next-event i3-ipc)
  (get-event i3-ipc))
