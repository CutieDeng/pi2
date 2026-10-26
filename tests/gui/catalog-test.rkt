#lang racket/base
;; tests/gui/catalog-test.rkt — 会话的组织目录（design-workspace.md W2）：只追加的记录重放 = 当前状态；多对多的边；
;; 删实体后边失效、会话不动；迁移（会话 → 主机 + 目录的工作区）幂等；同 git 远端的工作区给出 super-workspace 建议；截断的尾行不影响前面的

(require rackunit racket/file racket/list "../../src/gui/compute/catalog.rkt")

(define dir (make-temporary-file "pi2-catalog-~a" 'directory))
(define path (build-path dir "catalog.rktd"))
(define S1 "local\u0001/d/a.rktd") (define S2 "local\u0001/d/b.rktd") (define S3 "srv\u0001/r/c.rktd")

(test-case "entities, many-to-many edges, session attrs; reopening the file replays to the same state"
  (define c (catalog-open path))
  (define p1 (catalog-add-entity! c "project" "pi2 GUI" (hasheq 'notes "Metal")))
  (define p2 (catalog-add-entity! c "project" "market-pit"))
  (define sw (catalog-add-entity! c "super-workspace" "pi2 repo"))
  (define w-local (catalog-ensure-workspace! c "local" "/Users/me/pi2"))
  (define w-srv (catalog-ensure-workspace! c "srv" "/home/me/pi2"))
  (check-equal? (catalog-ensure-workspace! c "local" "/Users/me/pi2") w-local)      ; 同主机同目录：同一个
  (catalog-link! c p1 sw) (catalog-link! c sw w-local) (catalog-link! c sw w-srv)
  (catalog-link! c p2 w-local)                                                       ; 一个工作区属于两个项目
  (catalog-link! c w-local S1 "primary") (catalog-link! c p2 S1)                     ; 会话直接挂到另一个项目
  (catalog-set-session! c S1 #:host "local" #:path "/d/a.rktd" #:attrs (hasheq 'pinned #t))
  (catalog-set-session! c S1 #:attrs (hasheq 'archived #f))
  (check-equal? (sort (map car (catalog-parents c w-local)) string<?) (sort (list sw p2) string<?))
  (check-equal? (sort (map car (catalog-parents c S1)) string<?) (sort (list w-local p2) string<?))
  (check-equal? (catalog-session-attr c S1 'pinned) #t)
  (check-equal? (catalog-session-attr c S1 'archived) #f)
  (check-equal? (hash-ref (catalog-entity c w-local) 'name) "pi2 @ local")
  ;; 重开：同样的状态
  (define c2 (catalog-open path))
  (check-equal? (map (lambda (e) (hash-ref e 'name)) (catalog-entities-of c2 "project")) '("market-pit" "pi2 GUI"))
  (check-equal? (sort (map car (catalog-children c2 sw)) string<?) (sort (list w-local w-srv) string<?))
  (check-equal? (catalog-session-attr c2 S1 'pinned) #t)
  (check-equal? (hash-ref (hash-ref (catalog-entity c2 p1) 'attrs) 'notes) "Metal")
  ;; 新实体的 id 不和旧的撞
  (define p3 (catalog-add-entity! c2 "project" "third"))
  (check-false (member p3 (list p1 p2 sw w-local w-srv)))
  ;; 改名、解边、删实体
  (catalog-update-entity! c2 p1 #:name "pi2" #:attrs (hasheq 'budget 5))
  (check-equal? (hash-ref (catalog-entity c2 p1) 'name) "pi2")
  (check-equal? (hash-ref (hash-ref (catalog-entity c2 p1) 'attrs) 'notes) "Metal")       ; 合并，不丢旧设置
  (catalog-unlink! c2 p2 S1)
  (check-equal? (map car (catalog-parents c2 S1)) (list w-local))
  (catalog-remove-entity! c2 sw)
  (check-equal? (catalog-children c2 sw) '())
  (check-equal? (map car (catalog-parents c2 w-local)) (list p2))                    ; 指向删掉的实体的边失效
  (check-equal? (map car (catalog-parents (catalog-open path) w-local)) (list p2)))

(test-case "migration: sessions go to the workspace of their host + workdir, once; super-workspace suggestions by git remote"
  (define c (catalog-open #f))                                                        ; 只在内存
  (catalog-assign-session! c S1 "local" "/Users/me/pi2")
  (catalog-assign-session! c S2 "local" "/Users/me/pi2")
  (catalog-assign-session! c S3 "srv" "/home/me/pi2")
  (catalog-assign-session! c S1 "local" "/elsewhere")                                ; 已有 primary：不动
  (define ws (catalog-entities-of c "workspace"))
  (check-equal? (length ws) 2)
  (define wl (hash-ref (findf (lambda (e) (equal? (hash-ref (hash-ref e 'attrs) 'host) "local")) ws) 'id))
  (check-equal? (sort (map car (catalog-children c wl)) string<?) (sort (list S1 S2) string<?))
  (check-equal? (catalog-super-suggestions c) '())
  (for ([e ws]) (catalog-update-entity! c (hash-ref e 'id) #:attrs (hasheq 'git_remote "git@github.com:me/pi2.git")))
  (define sug (catalog-super-suggestions c))
  (check-equal? (map car sug) '("github.com/me/pi2"))
  (check-equal? (length (cdar sug)) 2)
  (define sw (catalog-add-entity! c "super-workspace" "pi2"))
  (for ([id (cdar sug)]) (catalog-link! c sw id))
  (check-equal? (catalog-super-suggestions c) '()))                                  ; 归过了：不再建议

(test-case "a truncated last line (crash while writing) keeps everything before it"
  (define p (build-path dir "trunc.rktd"))
  (define c (catalog-open p))
  (catalog-add-entity! c "project" "kept")
  (with-output-to-file p #:exists 'append (lambda () (display "#hasheq((op . \"entity\") (id . \"p9\" ")))
  (check-equal? (map (lambda (e) (hash-ref e 'name)) (catalog-entities-of (catalog-open p) "project")) '("kept")))

(define (line-count p) (if (file-exists? p) (length (file->lines p)) 0))

(test-case "remote URLs: ssh / https / scp forms of the same repo normalize to one key"
  (check-equal? (map normalize-remote '("git@github.com:me/pi2.git" "https://github.com/me/pi2" "ssh://git@GitHub.com:22/me/pi2.git/"
                                        "https://user@github.com/me/pi2.git" "/srv/git/repo.git" "file:///srv/git/repo"))
                '("github.com/me/pi2" "github.com/me/pi2" "github.com/me/pi2" "github.com/me/pi2" "/srv/git/repo" "/srv/git/repo")))

(test-case "sync a host's session list: workdir → workspace, chat → the host's chat workspace, ephemeral skipped, git remote recorded; idempotent; one write per sync"
  (define p (build-path dir "sync.rktd"))
  (define c (catalog-open p))
  (define rows (list (hasheq 'path "/d/a.rktd" 'workdir "/Users/me/pi2" 'chat #f 'git_remote "git@github.com:me/pi2.git")
                     (hasheq 'path "/d/b.rktd" 'workdir "/Users/me/pi2" 'chat #f 'git_remote 'null)
                     (hasheq 'path "/d/c.rktd" 'workdir "/data/chats/c" 'chat #t)
                     (hasheq 'path "/d/e.rktd" 'workdir "/Users/me/pi2" 'chat #f 'ephemeral #t)
                     (hasheq 'path "/d/x.rktd" 'workdir 'null 'chat #f)))
  (catalog-sync-host! c "local" rows)
  (define n1 (line-count p))
  (define ws (catalog-entities-of c "workspace"))
  (check-equal? (map (lambda (e) (hash-ref e 'name)) ws) '("chat @ local" "pi2 @ local"))
  (define-values (w1 _s1 _p1) (catalog-session-groups c (session-id "local" "/d/a.rktd")))
  (check-equal? (map (lambda (e) (hash-ref (hash-ref e 'attrs) 'git_remote #f)) w1) '("git@github.com:me/pi2.git"))
  (define-values (wc _s2 _p2) (catalog-session-groups c (session-id "local" "/d/c.rktd")))
  (check-equal? (map (lambda (e) (hash-ref (hash-ref e 'attrs) 'chat #f)) wc) '(#t))
  (check-equal? (catalog-parents c (session-id "local" "/d/e.rktd")) '())             ; 临时会话不挂
  (check-equal? (catalog-parents c (session-id "local" "/d/x.rktd")) '())             ; 没有工作目录
  (catalog-sync-host! c "local" rows)
  (check-equal? (line-count p) n1)                                                     ; 第二次：什么都不写
  ;; 一次同步写一次文件：记录都在，重开一样
  (check-equal? (map (lambda (e) (hash-ref e 'name)) (catalog-entities-of (catalog-open p) "workspace")) '("chat @ local" "pi2 @ local"))
  ;; 删会话 → 解边
  (catalog-forget-session! c (session-id "local" "/d/b.rktd"))
  (check-equal? (catalog-parents c (session-id "local" "/d/b.rktd")) '())
  (check-equal? (catalog-parents (catalog-open p) (session-id "local" "/d/b.rktd")) '()))

(test-case "session groups: projects reached directly, through the workspace, and through the super-workspace"
  (define c (catalog-open #f))
  (define sid (session-id "local" "/d/a.rktd"))
  (catalog-assign-session! c sid "local" "/Users/me/pi2")
  (define w (catalog-ensure-workspace! c "local" "/Users/me/pi2"))
  (define sw (catalog-add-entity! c "super-workspace" "pi2 repo"))
  (define pa (catalog-add-entity! c "project" "A")) (define pb (catalog-add-entity! c "project" "B")) (define pc (catalog-add-entity! c "project" "C"))
  (catalog-link! c sw w) (catalog-link! c pa sid) (catalog-link! c pb w) (catalog-link! c pc sw)
  (define-values (ws sws ps) (catalog-session-groups c sid))
  (check-equal? (map (lambda (e) (hash-ref e 'name)) ws) '("pi2 @ local"))
  (check-equal? (map (lambda (e) (hash-ref e 'name)) sws) '("pi2 repo"))
  (check-equal? (map (lambda (e) (hash-ref e 'name)) ps) '("A" "B" "C")))

(test-case "snapshot → in-memory catalog with the same state (replay starts from it); writes after it stay in memory"
  (define c (catalog-open #f))
  (catalog-sync-host! c "local" (list (hasheq 'path "/d/a.rktd" 'workdir "/w" 'chat #f)))
  (define p1 (catalog-add-entity! c "project" "P"))
  (catalog-link! c p1 (session-id "local" "/d/a.rktd"))
  (catalog-set-session! c (session-id "local" "/d/a.rktd") #:attrs (hasheq 'pinned #t))
  (define c2 (catalog-from-snapshot (catalog-snapshot c)))
  (check-false (catalog-path c2))
  (check-equal? (catalog-session-attr c2 (session-id "local" "/d/a.rktd") 'pinned) #t)
  (check-equal? (sort (map car (catalog-parents c2 (session-id "local" "/d/a.rktd"))) string<?) (sort (list p1 (catalog-ensure-workspace! c "local" "/w")) string<?))
  (check-equal? (catalog-ensure-workspace! c2 "local" "/w") (catalog-ensure-workspace! c "local" "/w"))   ; 索引也恢复了
  (check-false (member (catalog-add-entity! c2 "project" "Q") (list p1 (catalog-ensure-workspace! c "local" "/w")))))

(test-case "M1: a session's instruction chain = projects → super-workspaces → its primary workspace (not visited ones, not the chat workspace); blank text skipped"
  (define c (catalog-open #f))
  (define p (catalog-add-entity! c "project" "Shop" (hasheq 'instructions "P")))
  (define p0 (catalog-add-entity! c "project" "Empty" (hasheq 'instructions "  ")))
  (define sw (catalog-add-entity! c "super-workspace" "shop repo" (hasheq 'instructions "SW")))
  (define w (catalog-ensure-workspace! c "local" "/r/web"))
  (define v (catalog-ensure-workspace! c "local" "/tmp/elsewhere"))
  (catalog-update-entity! c w #:attrs (hasheq 'instructions "W"))
  (catalog-update-entity! c v #:attrs (hasheq 'instructions "V"))
  (catalog-link! c p sw) (catalog-link! c p0 w) (catalog-link! c sw w)
  (catalog-link! c w S1 "primary") (catalog-link! c v S1 "visited")
  (check-equal? (map (lambda (e) (catalog-entity-instructions e)) (catalog-instruction-chain c S1)) '("P" "SW" "W"))
  (define chat (catalog-ensure-chat-workspace! c "local"))
  (catalog-update-entity! c chat #:attrs (hasheq 'instructions "CHAT"))
  (catalog-link! c chat S2 "primary") (catalog-link! c p S2)
  (check-equal? (map catalog-entity-instructions (catalog-instruction-chain c S2)) '("P"))
  (check-false (catalog-entity-instructions (catalog-entity c p0)))
  ;; 还没进目录的新会话：按主机 + 工作目录找已有的工作区（只读）→ 同样的链；聊天的新会话 → 只有项目（这里没有）
  (define S9 "local\u0001/d/new.rktd")
  (check-equal? (catalog-instruction-chain c S9) '())
  (check-equal? (map catalog-entity-instructions (catalog-instruction-chain c S9 #:host "local" #:dir "/r/web")) '("P" "SW" "W"))
  (check-equal? (catalog-instruction-chain c S9 #:host "local" #:dir "/nowhere") '())
  (check-equal? (catalog-instruction-chain c S9 #:host "local" #:chat? #t) '())
  (check-false (catalog-entity c (format "w~a" 99))))

(delete-directory/files dir)
