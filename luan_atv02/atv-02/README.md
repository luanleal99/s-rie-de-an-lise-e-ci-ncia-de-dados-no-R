# Atividade 02 - Análise de Dados Palmer Penguins

**Aluno:** Luan Leal Serrão Ferraz  
**Matrícula:** 127043  

## Sobre o Projeto
Este projeto analisa dados morfológicos de três espécies de pinguins (*Pygoscelis adeliae*, *Pygoscelis papua* e *Pygoscelis antarcticus*) coletados no Arquipélago Palmer, Antártida.

## Estrutura do Repositório
- `data/raw/`: Contém a cópia bruta dos dados (`penguins.csv`).
- `data/processed/`: Contém os dados filtrados sem valores $NA$ (`penguins_clean.csv`).
- `scripts/`:
  - `01-importacao.R`: Exporta os dados brutos da biblioteca.
  - `02-limpeza.R`: Identifica ausências e limpa o dataset.
  - `03-analise.R`: Responde a perguntas estatísticas descritivas.
  - `04-visualizacao.R`: Gera e salva gráficos na pasta de saídas.
- `outputs/`: Armazena as figuras e gráficos gerados.

## Referência Bibliográfica
Gorman KB, Williams TD, Fraser WR (2014). Ecological Sexual Dimorphism and Environmental Variability within a Community of Antarctic Penguins (Genus *Pygoscelis*). PLoS ONE 9(3): e90081. https://doi.org/10.1371/journal.pone.0090081