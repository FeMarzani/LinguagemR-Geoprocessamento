library(rgdal)
library(sf)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(units)
library(viridis)
library(ggcorrplot)
library(GGally)
library(corrgram)

# COMANDO PARA LIMPEZA DE ENVIRONMENT
rm(list=ls())
rm(list=c('correlacao_municipios'))

# ARQUIVO GEO PACKAGE, COMANDO PARA LISTAR E VER O QUE TEM DENTRO E 
# CARREGAR ARQUIVOS

ogrListLayers("arquivostcc.gpkg")

areas_verdes <- st_read("arquivostcc.gpkg", layer = "areasverdes")
municipios_sul <- st_read("arquivostcc.gpkg", layer = "municipiossul")
dadoscovid <- st_read("arquivostcc.gpkg", layer = "dadoscovid")
estados<-st_read("arquivostcc.gpkg", layer = "Estados")
mesorregiao <- st_read("arquivostcc.gpkg", layer = "BR_Mesorregioes_2020") 
dadosmeso <- st_read("arquivostcc.gpkg", layer = "mesoregioes_sul DTB_2021_Municipio") 

# FILTRANDO OS DADOS DA COVID PARA A ULTIMA DATA

dadoscovid <- subset(dadoscovid, data==max(data))

# SELECIONANDO E CONVERTENDO VARIÁVEIS

dadoscovid<-dadoscovid%>%transmute(codmun=as.numeric(codmun), 
                                   municipio=municipio,
                                   coduf=coduf,
                                   estado=estado,
                                   pop=as.numeric(populacaoTCU2019), 
                                   casos=as.numeric(casosAcumulado),
                                   obitos=as.numeric(obitosAcumulado))

municipios_sul<-municipios_sul%>%transmute(codmun=as.numeric(cd_mun),
                                           municipio=nm_mun,
                                           estado=sigla,
                                           areamunicipio=areamuni)

# AGREGAÇÃO E SPATIAL JOIN

municipios_sul <- left_join(x=municipios_sul, y=dadoscovid, by=c('municipio','estado'))

areaverde<-st_join(areas_verdes,municipios_sul%>%select(municipio, areamunicipio),
                   join=st_intersects)

# ANULANDO GEOMETRIA DAS ÁREAS VERDES PARA SE PODER AGREGAR OS DADOS EM NOVA CAMADA

areaverde$geom <- NULL

mun_areaverde<-areaverde%>%group_by(municipio)%>%
  summarise(areaverde=sum(as.numeric(area)*100))

municipios_sul<-left_join(municipios_sul,mun_areaverde, by='municipio')

municipios_sul<-municipios_sul%>%mutate(areamunicipio=as.numeric(municipios_sul$areamunicipio *100),
                                        areaverdep=areaverde/as.numeric(municipios_sul$areamunicipio),
                                        areaverdehab=areaverde/pop)

# TUDO O Q FOR NA VAI SER ZERO:
municipios_sul[is.na(municipios_sul)] <- 0

municipios_final<-municipios_sul

municipios_final<-municipios_final%>%transmute(codmun.x=as.numeric(codmun.x), 
                                   municipio=municipio,
                                   codmun.y=codmun.y,
                                   coduf=coduf,
                                   estado=estado,
                                   pop=as.numeric(pop), 
                                   casos=as.numeric(casos),
                                   areaverde=as.numeric(areaverde),
                                   areaverdep=as.numeric(areaverdep),
                                   areaverdehab=as.numeric(areaverdehab),
                                   tx_casos=as.numeric((casos/pop)*100000))

# MUNICÍPIOS_FINAL É O ARQUIVO FINAL REFERENTE AOS DADOS AGREGADOS NOS MUNICÍPIOS
# PARA AS MESORREGIÕES É NECESSÁRIO FAZER AS AGREGAÇÕES DESSES DADOS NAS CAMADAS

muni_para_meso <- municipios_sul

muni_para_meso$geom <- NULL

head(muni_para_meso)
head(dadosmeso)

# ALTERANDO NOME DE COLUNAS PARA LEFT JOIN E MUDANDO CLASSE
names(dadosmeso)[12] <- "codmuni"
names(dadosmeso)[13] <- "municipio"
names(muni_para_meso)[1] <- "codmuni"

muni_para_meso$codmuni <- as.numeric(muni_para_meso$codmuni)
dadosmeso$codmuni <- as.numeric(dadosmeso$codmuni)

meso_dados <- left_join(muni_para_meso, dadosmeso, by=c('municipio','codmuni'))

names(mesorregiao)[1] <- 'Field3'
names(mesorregiao)[2] <- 'Field8'

# FAZENDO AGORA OS JOINS PARA SE OBTER O ARQUIVO FINAL DAS MESORREGIOES

meso_inicial <- left_join(meso_dados, mesorregiao, by=c('Field8','Field3'))

meso_inicial <- meso_inicial%>%group_by(Field8)%>%
  summarise(areasetor = sum(areamunicipio),
            areaverde = sum(areaverde),
            pop = sum(pop),
            casos = sum(casos))

meso_final <- merge(mesorregiao, meso_inicial, by='Field8')

meso_final<-meso_final%>%transmute(Field8 = Field8,
                                Field3= Field3,
                                sigla_uf = sigla_uf,
                                pop = pop,
                                casos = casos,
                                areaverde = areaverde,
                                tx_casos = (casos * 100000)/pop,
                                areaverdehab = (areaverde/pop)*100000,
                                areaverdep = areaverde/areasetor,
                                areasetor = areasetor)

# O ARQUIVO FINAL DAS MESORREGIOES É O MESO_FINAL

# PARA FAZER PARA OS ESTADOS OBSERVA-SE AS LINHAS ABAIXO

muni_para_estado <- municipios_sul
muni_para_estado$geom <- NULL

estado_teste<-muni_para_estado%>%group_by(estado)%>%
  summarise(areaestado = sum(areamunicipio),
            areaverde = sum(areaverde),
            pop = sum(pop),
            casos = sum(casos))

names(estado_teste)[1] <- "sigla"

estado_final<-left_join(estado_teste,estados, by='sigla')

estado_final<-estado_final%>%transmute(cod_uf=as.numeric(cod_uf), 
                           nome=nome,
                           cod_uf=cod_uf,
                           estado=sigla,
                           pop=as.numeric(pop), 
                           casos=as.numeric(casos),
                           tx_casos=as.numeric(casos)*100000/as.numeric(pop),
                           areaestado = areaestado,
                           areaverde = areaverde,
                           areaverdep = (areaverde/as.numeric(areaestado))*100000,
                           areaverdehab = areaverde/as.numeric(pop))

# ESTADO_FINAL É O ARQUIVO FINAL DOS ESTADOS.
# OS MAPAS COROPLETICOS DO TRABALHO FORAM FEITOS UTILIZANDO ESTES ARQUIVOS FINAIS
# APESAR DE SER POSSIVEL GERAR MAPAS ASSIM NO R, REALIZEI A EXPORTAÇÃO DESSES
# ARQUIVOS E FIZ OS MAPAS NO QGIS, PARA PODER OS ESTILIZAR MELHOR.
# ADICIONALMENTE, GEREI DOIS CORRELOGRAMAS AINDA NO R ATRAVÉS DOS CÓDIGOS ABAIXO

# CORRELOGRAMA E CORRELAÇÃO PARA MUNICIPIOS

correlacao_municipios <- municipios_final[,c(7, 8, 11, 9, 10)]
correlacao_municipios$geom <- NULL

ggcorr(correlacao_municipios, label = TRUE, method = c("everything", "spearman"))
ggsave('muni_correlacao.jpeg', width=17, height=12, dpi=300, units='cm')

cor.test(correlacao_municipios$areaverdep,correlacao_municipios$tx_casos, method="spearman")
cor.test(correlacao_municipios$areaverdehab,correlacao_municipios$tx_casos, method="spearman")
cor.test(correlacao_municipios$areaverde,correlacao_municipios$casos, method="spearman")

# CORRELOGRAMA E CORRELAÇÃO PARA MESORREGIOES

correlacao_mesoregioes <- meso_final[,c(5, 6, 7, 8, 9)]
correlacao_mesoregioes$geometry <- NULL

ggcorr(correlacao_mesoregioes, label = TRUE, method = c("everything", "spearman"))
ggsave('meso_correlacao.jpeg', width=17, height=12, dpi=300, units='cm')
