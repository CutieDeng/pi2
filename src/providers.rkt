#lang tstring racket
;; providers.rkt — 内置命名供应商档案（profile）。
;; 主档案 lmstudio（本地 LM Studio，无需密钥）；另加各大 code-plan 云供应商：
;; openai / anthropic / gemini / grok。openai/gemini/grok 均走 OpenAI 兼容线路，
;; 仅 endpoint/model/密钥不同；anthropic 走原生 Messages 线路（provider-anthropic.rkt）。
;;
;; 档案只描述后端；切换时把 endpoint/api-key(从环境变量)/model 写进 config，
;; 而 wire 格式由注册进 host providers 表的工厂（openai vs anthropic）决定。
;; provider 运行时读 current-config → /model、/provider 切换即时生效。

(require
 (file "model.rkt")
 (file "provider.rkt")
 (file "provider-anthropic.rkt")
 (file "plugin.rkt")                    ; plugin-host-providers（struct-out 已导出）
 (file "credentials.rkt")               ; resolve-key（env > 凭据文件）
) ; end require

;; kind: 'openai | 'anthropic（线路格式）
(struct provider-profile (name kind endpoint model key-env) #:transparent)

;; 默认 model 只是切换时的起点，可再用 --model / /model 覆盖。
(define BUILTIN-PROFILES
  (list
   (provider-profile "lmstudio"  'openai    "http://localhost:1234/v1"
                     "gemma-4-31b-it@6bit" #f)
   (provider-profile "openai"    'openai    "https://api.openai.com/v1"
                     "gpt-5" "OPENAI_API_KEY")
   (provider-profile "anthropic" 'anthropic "https://api.anthropic.com"
                     "claude-sonnet-5" "ANTHROPIC_API_KEY")
   (provider-profile "gemini"    'openai    "https://generativelanguage.googleapis.com/v1beta/openai"
                     "gemini-2.0-flash" "GEMINI_API_KEY")
   (provider-profile "grok"      'openai    "https://api.x.ai/v1"
                     "grok-4" "XAI_API_KEY")
   ;; DeepSeek 提供 Anthropic 兼容端点（/anthropic/v1/messages），故走原生 anthropic 线路，
   ;; 复用 provider-anthropic.rkt 的 x-api-key 鉴权与 SSE 累加器，零新线路代码。
   (provider-profile "deepseek"  'anthropic "https://api.deepseek.com/anthropic"
                     "deepseek-chat" "DEEPSEEK_API_KEY")
  ) ; end list
) ; end define BUILTIN-PROFILES

(define (profile-by-name name)
  (findf (lambda (p) (string=? (provider-profile-name p) name)) BUILTIN-PROFILES)
) ; end define profile-by-name

(define (builtin-provider-name? name) (and (profile-by-name name) #t))

(define (builtin-provider-names) (map provider-profile-name BUILTIN-PROFILES))

;; ---------------------------------------------------------------- 供应商实例
;; 实例名形如 "deepseek[work]"：同一 provider 挂多套 token，视作不同实例。
;; 无方括号 → 标签 "default"。base 决定线路/端点/默认模型；label 决定用哪套 token。

;; "deepseek[work]" → (values "deepseek" "work")；"deepseek" → (values "deepseek" "default")
(define (parse-instance name)
  (define m (regexp-match #rx"^([^][]+)\\[([^][]*)\\]$" name))
  (if m
      (values (cadr m) (let ([l (caddr m)]) (if (string=? l "") "default" l)))
      (values name "default"))
) ; end define parse-instance

(define (instance-base name) (let-values ([(b _l) (parse-instance name)]) b))

;; 规范显示名：default 标签隐去 → "deepseek"；否则 "deepseek[work]"。
(define (instance-display base label)
  (if (string=? label "default") base (string-append base "[" label "]"))
) ; end define instance-display

;; base 是否内置档案名（实例名亦可，取其 base 判定）。
(define (builtin-provider-instance? name) (builtin-provider-name? (instance-base name)))

;; 解析某实例的 token：实例文件密钥优先；无则 default 标签回退 profile 的 env 变量；再无 → #f。
(define (resolve-provider-token base label)
  (or (resolve-instance-key base label)
      (and (string=? label "default")
           (let ([env (provider-profile-key-env-of base)]) (and env (resolve-key env))))))

;; profile → (config → provider)：线路格式按 kind 选，读 current-config 取 live 值。
(define (profile->factory p)
  (case (provider-profile-kind p)
    [(anthropic) (lambda (cfg) (make-anthropic-provider cfg))]
    [else        (lambda (cfg) (make-openai-provider cfg))]
  ) ; end case
) ; end define profile->factory

;; 把内置档案注册进 host 的 providers 表（名→工厂）。幂等；插件同名注册可覆盖。
(define (register-builtin-providers! host)
  (for ([p (in-list BUILTIN-PROFILES)])
    (hash-set! (plugin-host-providers host)
               (provider-profile-name p)
               (profile->factory p)))
) ; end define register-builtin-providers!

;; 切到某内置档案/实例：把 endpoint/api-key/model 写进 cfg。未知名 → 原样返回。
;; name 可为实例名（"deepseek[work]"）：base 定端点/模型，label 定用哪套 token。
;; 密钥解析：实例文件密钥 > (default) env > #f（缺失时云端将 401；调用方可据 key-env 提示）。
(define (apply-provider-profile cfg name)
  (define-values (base label) (parse-instance name))
  (define p (profile-by-name base))
  (if p
      (struct-copy config cfg
                   [endpoint (provider-profile-endpoint p)]
                   [api-key (resolve-provider-token base label)]
                   [model (provider-profile-model p)])
      cfg)
) ; end define apply-provider-profile

;; profile 的密钥环境变量名（缺失提示用）；无则 #f。
(define (provider-profile-key-env-of name)
  (define p (profile-by-name name))
  (and p (provider-profile-key-env p))
) ; end define provider-profile-key-env-of

(provide
 (struct-out provider-profile)
 BUILTIN-PROFILES
 profile-by-name
 builtin-provider-name?
 builtin-provider-names
 register-builtin-providers!
 apply-provider-profile
 provider-profile-key-env-of
 parse-instance
 instance-base
 instance-display
 builtin-provider-instance?
 resolve-provider-token
) ; end provide
