CREATE TABLE IF NOT EXISTS avaliacoes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  
  sistema_nome TEXT NOT NULL,
  
  avaliador_nome TEXT NOT NULL,
  
  tarefa_nota INTEGER,
  tarefa_melhoria TEXT,
  
  auto_nota INTEGER,
  auto_melhoria TEXT,
  
  controle_nota INTEGER,
  controle_melhoria TEXT,
  
  expectativa_nota INTEGER,
  expectativa_melhoria TEXT,
  
  tolerancia_nota INTEGER,
  tolerancia_melhoria TEXT,
  
  individualizacao_nota INTEGER,
  individualizacao_melhoria TEXT,
  
  aprendizado_nota INTEGER,
  aprendizado_melhoria TEXT,
  
  media_avaliacao REAL
);
