# Indicadores Legais e Gerenciais

Gerador de relatórios de indicadores legais e gerenciais

## Roteiro de uso

### Origem dos dados

Os dados utilizados são os do SIAPC/PAD convertidos para CSV com [pad-converter](https://github.com/iddrs/pad-converter).

Os dados são importados para os arquivos `*indicadores*.xlsx`, que fazem os cálculos dos indicadores. Eventualmente, alguns valores precisam ser incluídos manualmente.

### Geração dos relatórios

Para gerar os relatórios *html* basta executar `RScript.exe *_to_html.R` usando o script *R* de acordo com a entidade desejada.

A geração em outros formatos, em especial o *pdf* pode ser feita, entretanto as tabelas e gráficos não ficam posissionados adequadamente 
por causa da forma que o *LaTeX* trabalha com figuras.

Os arquivos gerados ficam no diretório `output`.