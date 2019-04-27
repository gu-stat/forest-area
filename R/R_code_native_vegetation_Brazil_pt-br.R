# ************************************************************************* ----
# Pacote extrafont                                                          ----
# ************************************************************************* ----

#install.packages("extrafont")

library("extrafont")

# ************************************************************************* ----
# Importar Fonte                                                            ----
# ************************************************************************* ----

# Instale a fonte Lato

# Rode font_import apenas uma vez:

# font_import()

# Verifique se Lato foi incluida
fonts()[grep("Lato", fonts())]

# Registre as fontes no R

loadfonts(device = "win")

# ************************************************************************* ----
# Outros Pacotes                                                            ----
# ************************************************************************* ----

#install.packages("openxlsx")
#install.packages("tidyverse")
#install.packages("brazilmaps")
#install.packages("gifski")

library("openxlsx")
library("tidyverse")
library("brazilmaps")
library("gifski")

# ************************************************************************* ----
# Importar os Dados                                                         ----
# ************************************************************************* ----

# Mudar formato dos numeros
options(scipen=10)

# Caminho para o Arquivo no GitHub
file_path_forest_area = paste0(
  "https://github.com/gu-stat/forest-area/blob/master/",
  "data/Dados_Cobertura_MapBiomas_3.1_Biomas-UF-Municipios_%20SITE_UNPROTECTED.xlsx",
  "?raw=true")

file_path_city_area = paste0(
  "https://github.com/gu-stat/forest-area/blob/master/",
  "data/AR_BR_RG_UF_MUN_2016.xlsx",
  "?raw=true")

# Importacao
df.original <- openxlsx::read.xlsx(file_path_forest_area, 
                                   sheet = "LAND COVER - MUN_UF")

df.city.area <- openxlsx::read.xlsx(file_path_city_area, 
                                    sheet = "AR_BR_MUN_2016")

## \__ Dados para Criar Mapa por Cidade ----

brazil.map <- get_brmap(geo = "City", class = "sf")

# ************************************************************************* ----
# Manipulacao dos Dados                                                     ----
# ************************************************************************* ----

## \__ Transforma Codigo da Cidade em Numerico ----

df.city.area2 <-
  df.city.area %>% 
  dplyr::mutate(CODIBGE = as.numeric(CD_GCMUN)) %>% 
  dplyr::select(CODIBGE, AR_MUN_2016)


## \__ Dados de Area de Cobertura de Florestas Natuais ----

df.nat.forest <- 
  df.original %>% 
  filter(nivel2 == "Floresta Natural") %>%
  select(COD_ESTADO, estado, CODIBGE, nivel2, nivel3, 
         paste0(seq(1985, 2017))
  ) %>%
  # SOMA OS DIFERENTES SUB-GRUPOS DE Floresta Natural
  group_by(COD_ESTADO, estado, CODIBGE) %>%
  summarize_at(vars(-nivel2,-nivel3),sum) %>%
  ungroup()

## \__ Juntar com Dados de Area da Cidade ----

df.nat.forest.AREA <- 
  df.nat.forest %>%
  # ADICIONA AREA DE CADA CIDADE NA BASE COM DADOS DE VEGETACAO 
  dplyr::left_join(x = ., y = df.city.area2, by = "CODIBGE") %>%
  # CRIA PERCENTUAIS
  ## FAZ x/100 PARA TRANSFORMAR DE HECTARES PARA KM2
  dplyr::mutate_at(.vars=vars(paste0(seq(1985,2017))), 
                   .fun = function(x) round(100*((x/100)/.$AR_MUN_2016),3)
  )


# VERIFICA SE ALGUM MUNICIPIO TEM MAIS DE 100%
greater.1 <- NULL 
for (i in 1985:2017) {
  greater.1 <- append(greater.1, which(df.nat.forest.AREA[,paste0(i)] >= 100))
}

greater.1 <- greater.1 %>% unlist() %>% unique()

## \__ Cria os limites de percentuais ----

df.nat.forest.AREA <- 
  df.nat.forest.AREA %>%
  dplyr::mutate_at(.vars=vars(paste0(seq(1985,2017))), 
                   .fun = function(x) cut(x,breaks=c(-Inf, 0, 1, seq(0, 100, 10)[-1]))
  )

# ************************************************************************* ----
# Mapa                                                                      ----
# ************************************************************************* ----

## \__ Funcao que junta os dados de vegetacao com os de posicao das cidades ----

create.map.df <- function(DATA){
  
  map <- join_data(map = brazil.map, data = DATA, by = c("City" = "CODIBGE"))  
  
  map.ggplot <- as(map, "Spatial")
  
  map.ggplot@data$id = row.names(map.ggplot@data)
  
  df.map.ggplot <- suppressMessages(ggplot2::fortify(map.ggplot))
  
  df.map.ggplot <- dplyr::left_join(df.map.ggplot, map.ggplot@data, by = "id")
  
  return(df.map.ggplot)
}

## \____ Base de dados Para criar o Mapa ----

df.map.ggplot <- create.map.df(df.nat.forest.AREA)

## \__ Map Colors ----

# USEI https://gka.github.io/palettes PARA OBTER UMA ESCALA DE CORES
# COM 12 INTERVALOS (MESMO NUMERO DE INTERVALOS DE
# cut(x,breaks=c(-Inf, 0, 1, seq(0, 100, 10)[-1])) )
# QUE VAI DE white ATE darkgreen QUE EH APROPRIADA PARA PESSOAS COM
# VARIOS TIPOS DE DALTONISMO 

map.colors <- c('#ffffff','#eaf1e7','#d4e2cf','#bfd4b8','#abc5a1','#96b78a',
                '#81a974','#6c9b5f','#588d4a','#428034','#28711d','#006400')

### \__ Rotulos para a Legenda ----

map.labels <- c("0", "", "10%", rep("",3), "50%", rep("",3), "90%", "")

## \__ Criar Tema para o Mapa ----

map.theme <-
  theme(
    text = element_text(size = 12, family = "Lato", color = "#f2efef"),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill = "#3a3935", color = NA), 
    panel.background = element_rect(fill = "#3a3935", color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(t = 0.25,r = 0.25,b = 0.25,l = 0.25,unit = "cm"),
    plot.title = element_text(margin = margin(t = 0.25, unit = "cm")),
    plot.caption = element_text(size = 10),
    legend.background = element_rect(fill = "#3a3935", color = "#3a3935"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.position = "left",
    legend.key = element_rect(color = "transparent"),
    legend.key.size = unit(0, units = "mm"),
    legend.key.width = unit(2, units = "mm")
  )

## \__ Plotar Mapa ----

### \____ Funcao para o GIF ----
plot.map.forest <- function(){
  for (i in 1985:2017) {
    
    lost.area.year.km2 <-
      round(
        sum(-(df.nat.forest %>% summarize_at(paste0(i),sum)/100),
            df.nat.forest %>% summarize_at(paste0(1985),sum)/100)
        ,2
      )
    
    lost.area.year.mi2 <- round(lost.area.year.km2/2.59, 2)
    
    map.forest <-
      # MAPA
      ggplot(mapping = aes(x = long, y = lat, group = group)) +
      # MAPA COM LINHAS DA CIDADE TRANSPARENTES
      geom_polygon(
        data = df.map.ggplot,
        aes_string(fill = paste0("X",i)),
        color = "transparent"
      ) +
      # LEGENDA
      scale_fill_manual(
        values = map.colors,
        drop = FALSE,
        na.value = "black",
        na.translate = FALSE,
        name = "",
        label = map.labels,
        guide = guide_legend(
          direction = "vertical",
          ncol = 1,
          reverse = TRUE,
          label.vjust = 2.5
        )
      ) +
      # LIMITES DO MAPA
      xlim(range(df.map.ggplot$long)) +
      ylim(range(df.map.ggplot$lat)) +
      coord_map() +
      # ADICIONAR TITULO E CAPTIONS
      labs(
        title = "Percentual da Área da Cidade coberta por Vegetação Nativa, Brasil.",
        caption = paste0("Mapa: Gustavo Varela-Alvarenga - ogustavo.com/pt \n \n ",
                         "Dados: MapBiomas v.3.1 - mapbiomas.org & ",
                         "IBGE - www.ibge.gov.br ")
      ) +
      # ADICIONAR TEMA
      map.theme +
      # ADICIONAR ANNOTATIONS
      ## PALAVRA ANO
      annotate(
        geom = "text", x = -44.1, y = 2.4, size = 4, family = "Lato",
        color = "#f2efef", label = "ano", angle = 90
      ) +
      ## VALOR DO ANO
      annotate(
        geom = "text", x = -40, y = 2.5, size = 10, family = "Lato",
        color = "#f2efef", label = paste0(i)
      ) +
      ## TITULO - TABELA A ESQUERDA
      annotate(
        geom = "text", x = -68.5, y = -26, size = 4, family = "Lato",
        color = "#f2efef", hjust = 0.5,
        label =
          paste0("Área Total de Vegetação \n", "Nativa Perdida Desde 1985")
      ) +
      ## LINHA ABAIXO TITULO
      annotate(
        geom = "segment", x = -64.5, xend = -72.5, y = -28, yend = -28,
        linetype = "solid", color = "#f2efef"
      ) +
      ## VALORES EM KMs QUADRADOS COM . PARA MARCAR OS MILHARES
      annotate(
        geom = "text", x = -68.5, y = -29, size = 4.5, family = "Lato",
        color = "#f2efef", hjust = 0.5,
        label =
          ifelse(lost.area.year.km2 == 0, "0",
                 paste0(prettyNum(round(lost.area.year.km2, 0),
                                  big.mark=".", decimal.mark = ","),
                        " km2")
          )
      ) +
      ## VALORES EM MILHAS QUADRADAS COM , PARA MARCAR OS MILHARES
      annotate(
        geom = "text", x = -68.5, y = -31, size = 4.5, family = "Lato",
        color = "#f2efef", hjust = 0.5,
        label =
          ifelse(lost.area.year.mi2 == 0, "0",
                 paste0(prettyNum(round(lost.area.year.mi2, 0), big.mark=","),
                        " milhas2")
          )
      ) +
      ## LINHA ABAIXO TABELA
      annotate(
        geom = "segment", x = -64.5, xend = -72.5, y = -32, yend = -32,
        linetype = "solid", color = "#f2efef"
      ) +
      ## TITULO - TABELA A DIREITA
      annotate(
        geom = "text", x = -39, y = -26, size = 4, family = "Lato",
        color = "#f2efef", hjust = 0.5,
        label =  paste0("Área Perdida em \n", "% da Área do(a)")
      ) +
      ## LINHA ABAIXO TITULO
      annotate(
        geom = "segment", x = -35, xend = -43, y = -28, yend = -28,
        linetype = "solid", color = "#f2efef"
      ) +
      ## ADICIONAR PALAVRA TEXAS
      annotate(
        geom = "text", x = -41, y = -29, size = 4.5, family = "Lato",
        color = "#f2efef", label =  "Texas        : "
      ) +
      ## ADICIONAR PERCENTUAL PARA TEXAS
      annotate(
        geom = "text", x = -34, y = -29, size = 4.5, family = "Lato",
        color = "#f2efef", hjust = 1,
        label =
          ifelse(
            lost.area.year.km2 == 0, "0%",
            paste0(round(lost.area.year.km2/696241, 2) * 100, "%")
          )
      ) +
      ## ADICIONAR PALAVRA ALEMANHA
      annotate(
        geom = "text", x = -41, y = -31, size = 4.5, family = "Lato",
        color = "#f2efef", label =  "Alemanha : "
      ) +
      ## ADICIONAR PERCENTUAL PARA ALEMANHA
      annotate(
        geom = "text", x = -34, y = -31, size = 4.5, family = "Lato",
        color = "#f2efef", hjust = 1,
        label =
          ifelse(
            lost.area.year.km2 == 0, "0%",
            paste0(round(lost.area.year.km2/357386, 2) * 100, "%")
          )
      ) +
      ## LINHA ABAIXO DA TABELA
      annotate(
        geom = "segment", x = -35, xend = -43, y = -32, yend = -32,
        linetype = "solid", color = "#f2efef"
      )
    
    print(map.forest)
    
  }
}

### \____ Criar GIF ----
### O comando abaixo salvara um arquivo gif de nome forest_animation_pt-br.gif 
### no seu working directory

gif_file <-
  gifski::save_gif(
    expr = plot.map.forest(),
    gif_file = "forest_animation_pt-br.gif",
    delay = 0.75, width = 738, height = 788, res = 100
  )

utils::browseURL(gif_file)

### \____ Criar Mapas Estaticos ----
#install.packages("Cairo")
library("Cairo")

for (i in 1985:2017) {
  
  lost.area.year.km2 <-
    round(
      sum(-(df.nat.forest %>% summarize_at(paste0(i),sum)/100),
          df.nat.forest %>% summarize_at(paste0(1985),sum)/100)
      ,2
    )
  
  lost.area.year.mi2 <- round(lost.area.year.km2/2.59, 2)
  
  map.forest <-
    # MAPA
    ggplot(mapping = aes(x = long, y = lat, group = group)) +
    # MAPA COM LINHAS DA CIDADE TRANSPARENTES
    geom_polygon(
      data = df.map.ggplot,
      aes_string(fill = paste0("X",i)),
      color = "transparent"
    ) +
    # LEGENDA
    scale_fill_manual(
      values = map.colors,
      drop = FALSE,
      na.value = "black",
      na.translate = FALSE,
      name = "",
      label = map.labels,
      guide = guide_legend(
        direction = "vertical",
        ncol = 1,
        reverse = TRUE,
        label.vjust = 2.5
      )
    ) +
    # LIMITES DO MAPA
    xlim(range(df.map.ggplot$long)) +
    ylim(range(df.map.ggplot$lat)) +
    coord_map() +
    # ADICIONAR TITULO E CAPTIONS
    labs(
      title = "Percentual da Área da Cidade coberta por Vegetação Nativa, Brasil.",
      caption = paste0("Mapa: Gustavo Varela-Alvarenga - ogustavo.com/pt \n \n ",
                       "Dados: MapBiomas v.3.1 - mapbiomas.org & ",
                       "IBGE - www.ibge.gov.br ")
    ) +
    # ADICIONAR TEMA
    map.theme +
    # ADICIONAR ANNOTATIONS
    ## PALAVRA ANO
    annotate(
      geom = "text", x = -44.1, y = 2.4, size = 4, family = "Lato",
      color = "#f2efef", label = "ano", angle = 90
    ) +
    ## VALOR DO ANO
    annotate(
      geom = "text", x = -40, y = 2.5, size = 10, family = "Lato",
      color = "#f2efef", label = paste0(i)
    ) +
    ## TITULO - TABELA A ESQUERDA
    annotate(
      geom = "text", x = -68.5, y = -26, size = 4, family = "Lato",
      color = "#f2efef", hjust = 0.5,
      label =
        paste0("Área Total de Vegetação \n", "Nativa Perdida Desde 1985")
    ) +
    ## LINHA ABAIXO TITULO
    annotate(
      geom = "segment", x = -64.5, xend = -72.5, y = -28, yend = -28,
      linetype = "solid", color = "#f2efef"
    ) +
    ## VALORES EM KMs QUADRADOS COM . PARA MARCAR OS MILHARES
    annotate(
      geom = "text", x = -68.5, y = -29, size = 4.5, family = "Lato",
      color = "#f2efef", hjust = 0.5,
      label =
        ifelse(lost.area.year.km2 == 0, "0",
               paste0(prettyNum(round(lost.area.year.km2, 0),
                                big.mark=".", decimal.mark = ","),
                      " km2")
        )
    ) +
    ## VALORES EM MILHAS QUADRADAS COM , PARA MARCAR OS MILHARES
    annotate(
      geom = "text", x = -68.5, y = -31, size = 4.5, family = "Lato",
      color = "#f2efef", hjust = 0.5,
      label =
        ifelse(lost.area.year.mi2 == 0, "0",
               paste0(prettyNum(round(lost.area.year.mi2, 0), big.mark=","),
                      " milhas2")
        )
    ) +
    ## LINHA ABAIXO TABELA
    annotate(
      geom = "segment", x = -64.5, xend = -72.5, y = -32, yend = -32,
      linetype = "solid", color = "#f2efef"
    ) +
    ## TITULO - TABELA A DIREITA
    annotate(
      geom = "text", x = -39, y = -26, size = 4, family = "Lato",
      color = "#f2efef", hjust = 0.5,
      label =  paste0("Área Perdida em \n", "% da Área do(a)")
    ) +
    ## LINHA ABAIXO TITULO
    annotate(
      geom = "segment", x = -35, xend = -43, y = -28, yend = -28,
      linetype = "solid", color = "#f2efef"
    ) +
    ## ADICIONAR PALAVRA TEXAS
    annotate(
      geom = "text", x = -41, y = -29, size = 4.5, family = "Lato",
      color = "#f2efef", label =  "Texas        : "
    ) +
    ## ADICIONAR PERCENTUAL PARA TEXAS
    annotate(
      geom = "text", x = -34, y = -29, size = 4.5, family = "Lato",
      color = "#f2efef", hjust = 1,
      label =
        ifelse(
          lost.area.year.km2 == 0, "0%",
          paste0(round(lost.area.year.km2/696241, 2) * 100, "%")
        )
    ) +
    ## ADICIONAR PALAVRA ALEMANHA
    annotate(
      geom = "text", x = -41, y = -31, size = 4.5, family = "Lato",
      color = "#f2efef", label =  "Alemanha : "
    ) +
    ## ADICIONAR PERCENTUAL PARA ALEMANHA
    annotate(
      geom = "text", x = -34, y = -31, size = 4.5, family = "Lato",
      color = "#f2efef", hjust = 1,
      label =
        ifelse(
          lost.area.year.km2 == 0, "0%",
          paste0(round(lost.area.year.km2/357386, 2) * 100, "%")
        )
    ) +
    ## LINHA ABAIXO DA TABELA
    annotate(
      geom = "segment", x = -35, xend = -43, y = -32, yend = -32,
      linetype = "solid", color = "#f2efef"
    )
  
  ### Observe que o comando abaixo ira salvar varios arquivos
  ### png com nome Y*_pt-br.png, onde * eh o ano, no seu 
  ### working directory
  
  ggsave(
    paste0("Y",i,"_pt-br.png"), device="png", type="cairo", 
    width = 7, height = 7
  )
  
}  