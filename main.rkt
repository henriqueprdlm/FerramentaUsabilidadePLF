#lang racket

(require racket/string)
(require web-server/servlet
         web-server/servlet-env
         web-server/http
         racket/bytes
         db
         net/url)

(define (string-blank? s)
  (and (string? s)
       (string=? (string-trim s) "")))

;; Conexão com o banco SQLite
(define conn (sqlite3-connect #:database "banco.sqlite"))

;; Estilização
(define css-estilo
  "
    * {
      box-sizing: border-box;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      color: #222;
      margin: 0;
      padding: 0;
    }

    body {
      background-color: #f4f7fc;
      padding: 20px;
    }

    h1 {
      text-align: center;
      margin-top: 10px;
      margin-bottom: 30px;
      font-size: 1.8rem;
    }

    form#avaliacao {
      background: #fff;
      max-width: 700px;
      margin: 0 auto;
      padding: 30px;
      border-radius: 8px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.05);
    }

    form div {
      margin-bottom: 20px;
      display: flex;
      flex-direction: column;
    }

    form span {
      font-size: 1rem;
      magin-bottom: 20px;
    }

    label, .title {
       font-weight: 600;
       font-size: 1.1rem;
       margin-bottom: 10px;
    }

    input, textarea {
      font-size: 0.8rem;
      padding: 6px 10px;
      border: 1px solid #ccc;
      border-radius: 4px;
      margin-top: 4px;
      margin-bottom: 10px;
    }

    textarea {
      resize: vertical;
    }

    div.radios {
      display: flex;
      flex-direction: column;
      margin: 15px 0;
    }

    div.radios label {
      font-size: 1rem;
      font-weight: 400;
      margin-bottom: 0px;
    }

    ul {
      list-style-type: none;
      padding: 0;
      max-width: 700px;
      margin: 0 auto;
    }

    li {
      background: #fff;
      padding: 15px 20px;
      border-radius: 8px;
      margin-bottom: 10px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.05);
    }

    a {
      text-decoration: none;
      color: #222;
      font-weight: 500;
    }

    a:hover {
      text-decoration: underline;
    }
    
    .box {
      max-width: 700px;
      margin: 10px auto;
      background: #fff;
      padding: 15px 20px;
      border-radius: 8px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.05);
    }

    #btn {
      font-size: 1rem;
      display: block;
      width: max-content;
      margin: 20px auto 0px auto;
      padding: 10px 20px;
      background-color: #333;
      color: white;
      text-align: center;
      border-radius: 8px;
      text-decoration: none;
      cursor: pointer;
    }

    #btn:hover {
      background-color: #4a4a4a;
    }

    table {
      width: 100%;
      max-width: 700px;
      margin: 0 auto 20px auto;
      border-collapse: collapse;
      background: #fff;
      border-radius: 8px;
      overflow: hidden;
      box-shadow: 0 4px 8px rgba(0,0,0,0.05);
      table-layout: fixed;
    }

    th, td {
      text-align: left;
      border-bottom: 1px solid #eee;
    }

    th {
      padding: 15px;
      background-color: #f7f7f7;
      font-weight: 600;
    }

    th:nth-child(1),
    td:nth-child(1) {
      width: 110px;
      white-space: nowrap;
      text-align: center;
    }

    th:nth-child(2),
    td:nth-child(2),
    th:nth-child(3),
    td:nth-child(3) {
      width: auto;
      word-wrap: break-word;
    }

    tr:hover {
      background-color: #f0f0f0;
      cursor: pointer;
    }

    a.row-link {
      display: block;
      color: inherit;
      text-decoration: none;
      width: 100%;
      height: 100%;
      padding: 15px;
    }

    form#form-excluir {
      display: flex;
      justify-content: flex-end;
      max-width: 700px;
      margin: 10px auto 0;
    }

    input#btn-excluir {
      background-color: transparent;
      border: none;
      color: #888;
      text-decoration: none;
      cursor: pointer;
      font-size: 0.9rem;
    }
    
    input#btn-excluir:hover {
      text-decoration: underline;
    }
  ")


;; Salvar avaliação
(define (salvar-avaliacao
         sistema_nome
         avaliador_nome
         tarefa_nota tarefa_melhoria
         auto_nota auto_melhoria
         controle_nota controle_melhoria
         expectativa_nota expectativa_melhoria
         tolerancia_nota tolerancia_melhoria
         individualizacao_nota individualizacao_melhoria
         aprendizado_nota aprendizado_melhoria
         media_avaliacao)
  (query-exec conn
    "INSERT INTO avaliacoes (
       sistema_nome,
       avaliador_nome,
       tarefa_nota, tarefa_melhoria,
       auto_nota, auto_melhoria,
       controle_nota, controle_melhoria,
       expectativa_nota, expectativa_melhoria,
       tolerancia_nota, tolerancia_melhoria,
       individualizacao_nota, individualizacao_melhoria,
       aprendizado_nota, aprendizado_melhoria,
       media_avaliacao
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    sistema_nome
    avaliador_nome
    tarefa_nota tarefa_melhoria
    auto_nota auto_melhoria
    controle_nota controle_melhoria
    expectativa_nota expectativa_melhoria
    tolerancia_nota tolerancia_melhoria
    individualizacao_nota individualizacao_melhoria
    aprendizado_nota aprendizado_melhoria
    media_avaliacao))

;; Formulário
(define (formulario req)
  (response/xexpr
   `(html
     (head (title "Inspeção de Usabilidade")
           (style ,css-estilo))
     (body
      (h1 "Avaliação de Usabilidade (ISO 9241-10)")

      (form ((action "/salvar") (method "post") (id "avaliacao"))
            (div
             (label "Nome do sistema")
             (input ((type "text") (name "sistema_nome") (required "required"))))
            (div
             (label "Nome do avaliador")
             (input ((type "text") (name "avaliador_nome") (required "required"))))
        ,@(for/list ([label '("Adequação à tarefa"
                              "Auto descrição"
                              "Controlabilidade"
                              "Conformidade com as expectativas do usuário"
                              "Tolerância ao erro"
                              "Adequação à individualização"
                              "Adequação ao aprendizado")]
                     [pergunta '("O sistema ajuda o usuário a realizar suas tarefas com rapidez e eficiência, sem exigir etapas desnecessário ou esforço adicional."
                                 "O sistema é compreensível de forma intuitiva, sem precisar de manuais ou instruções externas."
                                 "Você está no controle da interação com o sistema, podendo iniciar, interromper ou desfazer ações a qualquer momento."
                                 "O sistema se comportar de acordo com suas experiências e costumes anteriores, seguindo padrões reconhecíveis."
                                 "O sistema evita erros sempre que possível e permite que que você se recupere facilmente caso eles ocorram."
                                 "O sistema permite personalização de acordo com as suas preferências ou necessidades."
                                 "O sistema é fácil de aprender, oferecendo elementos que facilitem esse processo.")]
                     [key '("tarefa" "auto" "controle" "expectativa" "tolerancia" "individualizacao" "aprendizado")])
            `(div
              (label ,label)
              (span ,pergunta)
              (div ((class "radios"))
                   ,@(for/list ([i '(0 1 2 3 4)]
                                [label '("Discordo totalmente" "Discordo parcialmente" "Não sei" "Concordo parcialmente" "Concordo totalmente")])
                       `(label
                         (input ((type "radio")
                                 (name ,(string-append key "_nota"))
                                 (value ,(number->string i))
                                 (required "required")))
                         ,(string-append " " label))))
              "Melhoria que pode ser feita ou comentário: " (textarea ((name ,(string-append key "_melhoria")) (rows "2") (cols "40")))))
        (input ((type "submit") (value "Salvar avaliação") (id "btn"))))

      (a ((href "/avaliacoes") (id "btn") (style "margin-right: 0;")) "Ver Avaliações")))))

;; Processar POST
(define (salvar req)
  (define raw-method (request-method req))
  (define method-str
    (cond
      [(symbol? raw-method) (symbol->string raw-method)]
      [(bytes?   raw-method) (bytes->string/utf-8 raw-method)]
      [(string?  raw-method) raw-method]
      [else ""]))

  (if (not (string-ci=? method-str "POST"))
      (response/xexpr
       `(html
         (head (title "Método não permitido"))
         (body
           (h1 "405 - Método não permitido")
           (p "Essa rota só pode ser acessada via POST."))))
      
      (let* ([bs     (request-bindings req)]
             [campos
              '(sistema_nome
                avaliador_nome
                tarefa_nota tarefa_melhoria
                auto_nota auto_melhoria
                controle_nota controle_melhoria
                expectativa_nota expectativa_melhoria
                tolerancia_nota tolerancia_melhoria
                individualizacao_nota individualizacao_melhoria
                aprendizado_nota aprendizado_melhoria)]
             [dados
              (for/list ([k campos])
                (let* ([b   (assoc k bs)]
                       [raw (and b (cdr b))]
                       [s   (cond
                              [(bytes? raw) (bytes->string/utf-8 raw)]
                              [(string? raw) raw]
                              [else (format "~a" raw)])])
                  (if (regexp-match? #px"_nota$" (symbol->string k))
                      (string->number (or s "0"))
                      s)))])

        (let* ([sistema-nome   (list-ref dados 0)]
               [avaliador-nome (list-ref dados 1)]
               [notas          (list (list-ref dados 2)   ; tarefa_nota
                                     (list-ref dados 4)   ; auto_nota
                                     (list-ref dados 6)   ; controle_nota
                                     (list-ref dados 8)   ; expectativa_nota
                                     (list-ref dados 10)  ; tolerancia_nota
                                     (list-ref dados 12)  ; individualizacao_nota
                                     (list-ref dados 14)  ; aprendizado_nota
                                     )]
               [soma  (apply + notas)]
               [media (/ (* soma 10) 28.0)]) 

          (if (and (not (string-blank? sistema-nome))
                   (not (string-blank? avaliador-nome))
                   (andmap number? notas))
              (begin
                (apply salvar-avaliacao (append dados (list media)))
                (redirect-to "/avaliacoes"))
              (response/xexpr
               `(html
                 (head  (title "Erro ao salvar"))
                 (body
                   (h1 "Erro ao salvar")
                   (p "Preencha o nome do sistema, nome do avaliador e selecione todas as notas.")))))))))

;; Listar
(define (listar req)
  (define rows (query-rows conn "SELECT id, sistema_nome, avaliador_nome FROM avaliacoes ORDER BY id"))
  (response/xexpr
   `(html
     (head (title "Avaliações")
           (style ,css-estilo))
     (body
      (h1 "Lista de Avaliações")
      (table
       (thead
        (tr
         (th "Avaliação")
         (th "Sistema")
         (th "Avaliador")))
       (tbody
        ,@(for/list ([r rows])
            (define id (vector-ref r 0))
            (define sistema (vector-ref r 1))
            (define avaliador (vector-ref r 2))
            `(tr
              (td (a ((href ,(format "/avaliacao?id=~a" id)) (class "row-link")) ,(number->string id)))
              (td (a ((href ,(format "/avaliacao?id=~a" id)) (class "row-link")) ,sistema))
              (td (a ((href ,(format "/avaliacao?id=~a" id)) (class "row-link")) ,avaliador))))))
      (a ((href "/") (id "btn")) "Nova Avaliação")))))

;; Detalhar
(define (detalhar req)
  (define id-str (extract-binding/single 'id (request-bindings req)))
  (define id (string->number id-str))
  (define row (query-row conn "SELECT * FROM avaliacoes WHERE id=?" id))
  (define sistema-nome (vector-ref row 1))
  (define avaliador-nome (vector-ref row 2))

  (define media_avaliacao (vector-ref row 17))
  (define media-int (inexact->exact (round (* media_avaliacao 100))))
  (define int-part (quotient media-int 100))
  (define frac-part (modulo media-int 100)) 
  (define frac-str
    (if (< frac-part 10)
        (string-append "0" (number->string frac-part))
        (number->string frac-part)))        
  (define media_texto
    (string-append (number->string int-part) "," frac-str))

  (define topicos
    `(("Adequação à tarefa" 3 4)
      ("Auto descrição" 5 6)
      ("Controlabilidade" 7 8)
      ("Conformidade com as expectativas do usuário" 9 10)
      ("Tolerância ao erro" 11 12)
      ("Adequação à individualização" 13 14)
      ("Adequação ao aprendizado" 15 16)))

  (response/xexpr
   `(html
     (head (title "Detalhes da Avaliação")
           (style ,css-estilo))
     (body
      (h1 ,(format "Avaliação ~a" id))

      (div ((class "box"))
           (strong "Nome do sistema: ") 
           (span ,sistema-nome))

      (div ((class "box"))
           (strong "Nome do avaliador: ") 
           (span ,avaliador-nome))

      ,@(for/list ([topico topicos])
           (define titulo (first topico))
           (define nota-idx (second topico))
           (define melhoria-idx (third topico))
           (define nota (vector-ref row nota-idx))
           (define nota-texto
             (case nota
               [(0) "Discordo totalmente"]
               [(1) "Discordo parcialmente"]
               [(2) "Não sei"]
               [(3) "Concordo parcialmente"]
               [(4) "Concordo totalmente"]
               [else "Desconhecida"]))
           `(div ((class "box"))
                 (p ((class "title")) (strong ,(string-append titulo)))
                 (p (strong "Nota (0 a 4): ") ,(format "~a (~a)" nota nota-texto))
                 (p (strong "Melhoria ou comentário: ") ,(format "~a" (vector-ref row melhoria-idx)))))

         (div ((class "box"))
              (strong "Média geral: ") 
              (span ,media_texto))

         (form ((action ,(format "/excluir?id=~a" id)) (method "post") (id "form-excluir"))
               (input ((type "submit") (value "Excluir Avaliação") (id "btn-excluir"))))

         (a ((href "/avaliacoes") (id "btn")) "Voltar à Lista")))))




;; Excluir avaliação
(define (excluir req)
  (define id-str (extract-binding/single 'id (request-bindings req)))
  (define id (string->number id-str))
  (query-exec conn "DELETE FROM avaliacoes WHERE id=?" id)
  (redirect-to "/avaliacoes"))


;; Dispatcher 
(define (disp req)
  (define path (url-path (request-uri req)))
  (define route (string-join (map path/param-path path) "/"))
  (cond
    [(string=? route "") (formulario req)]
    [(string=? route "salvar") (salvar req)]
    [(string=? route "avaliacoes") (listar req)]
    [(string=? route "avaliacao") (detalhar req)]
    [(string=? route "excluir") (excluir req)]
    [else
     (response/xexpr
      `(html
        (head (title "404 – Não encontrado"))
        (body
         (h1 "404 – Página não encontrada")
         (p "A rota \"" ,route "\" não existe.")
         (a ((href "/")) "Voltar"))))]))

;; Inicia o servidor
(serve/servlet disp
               #:launch-browser? #t
               #:port 8000
               #:servlet-path "/"
               #:servlet-regexp #rx"")

