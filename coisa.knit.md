---
title: "coisa"
output:
  pdf_document: default
  html_document: default
---




```r
# Idade que tem quando foi internado
ug1.baseline=urg1[!duplicated(urg1$Ep.int),]

idade.int=NULL
for (i in 1:length(ug1.baseline$Data.int)) {
  idade.int[i]=length(seq(ug1.baseline$Data.nasc[i],ug1.baseline$Data.int[i],by="year"))-1
}
ug1.baseline$idade.int=idade.int
head(ug1.baseline,15)
```

```
##     Ep.int N.processo     Ep.urg Sexo  Data.nasc Idade            Concelho
## 1  1338405     298521  101013100    M 1959-09-14    58              AMARES
## 5  1338423   24006236  101013439    M 1945-08-06    72     TERRAS DE BOURO
## 12 1344670   40285741  101040329    F 1947-04-30    70           ESPOSENDE
## 14 1345067   40250062 Programada    M 2015-09-08     2               BRAGA
## 15 1348768   40032441  101056551    M 1954-10-27    63            BARCELOS
## 19 1349369   92031975  101058606    F 1935-02-25    83    POVOA DE LANHOSO
## 23 1350191   40289994  101062669    M 1972-11-12    45 VILA NOVA FAMALICAO
## 25 1350561   24005481  101063994    M 1952-02-08    66          VILA VERDE
## 29 1351624   40249231  101067964    F 1949-03-24    68            BARCELOS
## 33 1353558   94001364  101076099    M 1955-11-10    62    POVOA DE LANHOSO
## 35 1353569   92115117  101075748    F 1936-03-16    81          VILA VERDE
## 38 1353677   93013836  101076877    F 1968-12-30    49          VILA VERDE
## 45 1353759   40292705  101077189    M 1968-10-13    49   VILA NOVA DE GAIA
## 47 1353919   92035026 Programada    F 1944-05-10    73                FAFE
## 48 1354224   92007415  101079012    F 1940-01-03    78               BRAGA
##    Distrito Escolaridade Estado.civil                       Profissao
## 1     BRAGA         <NA>            C                    DESEMPREGADO
## 5     BRAGA         <NA>            C                      AGRICULTOR
## 12    BRAGA         <NA>         <NA>                            <NA>
## 14    BRAGA         <NA>            S                            <NA>
## 15    BRAGA         <NA>            V                            <NA>
## 19    BRAGA         <NA>         <NA>                            <NA>
## 23    BRAGA         <NA>            C                       MOTORISTA
## 25    BRAGA         <NA>            C TRABALHADOR DA CONSTRUÇÃO CIVIL
## 29    BRAGA         <NA>            C                            <NA>
## 33    BRAGA         <NA>            C                            <NA>
## 35    BRAGA         <NA>            C                       DOMÉSTICA
## 38    BRAGA         <NA>            C                            <NA>
## 45    PORTO         <NA>         <NA>                            <NA>
## 47    BRAGA         <NA>            C                            <NA>
## 48    BRAGA         <NA>            C                            <NA>
##    Tipo.producao Ind.urgencia   Data.int
## 1        Urgente            S 2016-07-10
## 5        Urgente            S 2016-07-11
## 12       Urgente            S 2016-08-25
## 14    Programada            N 2016-08-29
## 15       Urgente            S 2016-09-25
## 19       Urgente            S 2016-09-28
## 23       Urgente            S 2016-10-05
## 25       Urgente            S 2016-10-08
## 29       Urgente            S 2016-10-15
## 33       Urgente            S 2016-10-29
## 35       Urgente            S 2016-10-28
## 38       Urgente            S 2016-10-31
## 45       Urgente            S 2016-10-31
## 47    Programada            N 2016-11-02
## 48       Urgente            S 2016-11-04
##                          Servico.admissao  Valencia.admissao
## 1                           Neurocirurgia         Neurologia
## 5           Int. - UC Intermédios Médicos   Medicina Interna
## 12                          Neurocirurgia      Neurocirurgia
## 14                              Pediatria          Pediatria
## 15          Int. - UC Intermédios Médicos   Medicina Interna
## 19                          Neurocirurgia      Neurocirurgia
## 23                     Medicina Intensiva Medicina Intensiva
## 25                     Medicina Intensiva Medicina Intensiva
## 29                       Medicina Interna   Medicina Interna
## 33                              Ortopedia          Ortopedia
## 35          Int. - UC Intermédios Médicos   Medicina Interna
## 38                          Neurocirurgia         Neurologia
## 45          Int. - UC Intermédios Médicos   Medicina Interna
## 47 Int. Piso 1-C - Oncologia e Nefrologia          Oncologia
## 48                       Medicina Interna   Medicina Interna
##                            Servico.fisico                       Valencia
## 1        Int. Piso 3-E - Neurologia e MFR Medicina Fisica e Reabilitação
## 5                          Cirurgia Geral                 Cirurgia Geral
## 12       Int. Piso 3-E - Neurologia e MFR                  Neurocirurgia
## 14                              Pediatria                      Pediatria
## 15          Int. - UC Intermédios Médicos               Medicina Interna
## 19                     Medicina Intensiva             Medicina Intensiva
## 23                     Medicina Intensiva             Medicina Intensiva
## 25          Int. - UC Intermédios Médicos               Medicina Interna
## 29          Int. - UC Intermédios Médicos               Medicina Interna
## 33          Int. - UC Intermédios Médicos               Medicina Interna
## 35          Int. - UC Intermédios Médicos               Medicina Interna
## 38       Int. Piso 3-E - Neurologia e MFR Medicina Fisica e Reabilitação
## 45          Int. - UC Intermédios Médicos               Medicina Interna
## 47 Int. Piso 1-C - Oncologia e Nefrologia                      Oncologia
## 48       Int. Piso 3-E - Neurologia e MFR Medicina Fisica e Reabilitação
##    Data.entrada Data.saida  Data.alta                   Servico.alta
## 1    2016-09-12 2016-09-13 2017-04-13 Medicina Fisica e Reabilitação
## 5    2016-08-31 2016-10-21 2017-03-22 Medicina Fisica e Reabilitação
## 12   2016-10-23 2016-11-28 2017-01-26                  Neurocirurgia
## 14   2016-08-29 2017-04-17 2017-04-17                      Pediatria
## 15   2016-09-25 2016-10-14 2017-01-13 Medicina Fisica e Reabilitação
## 19   2016-09-29 2016-10-04 2017-01-05 Medicina Fisica e Reabilitação
## 23   2016-10-05 2016-10-19 2017-03-15                  Neurocirurgia
## 25   2016-10-20 2016-10-24 2017-03-20                      Ortopedia
## 29   2016-10-26 2017-01-05 2017-02-08                    Pneumologia
## 33   2016-10-30 2016-12-01 2017-02-03                      Ortopedia
## 35   2016-10-28 2016-11-27 2017-01-13               Medicina Interna
## 38   2016-12-16 2016-12-21 2017-04-13 Medicina Fisica e Reabilitação
## 45   2016-10-31 2016-11-02 2017-01-06               Medicina Interna
## 47   2016-11-02 2017-02-20 2017-02-20                      Oncologia
## 48   2016-12-19 2016-12-21 2017-02-10 Medicina Fisica e Reabilitação
##        ICM
## 1  12.8869
## 5  12.8869
## 12  3.6764
## 14  1.2773
## 15 12.8869
## 19 12.8869
## 23 12.8869
## 25  3.1446
## 29 12.8869
## 33  3.2707
## 35  3.8546
## 38  3.6764
## 45  1.1208
## 47  5.8904
## 48  1.1340
##                                                                                                                                                                         GDH
## 1  Oxigenação por membrana extra-corporal, traqueostomia com ventilação mecânica >96h ou traqueostomia com outro diagnóstico principal, excepto da face, boca ou do pescoço
## 5  Oxigenação por membrana extra-corporal, traqueostomia com ventilação mecânica >96h ou traqueostomia com outro diagnóstico principal, excepto da face, boca ou do pescoço
## 12                                                                                                                                                Craniotomia com CC  major
## 14                                                                                          Perturbações respiratórias, excepto infecções, bronquite ou asma, com CC  major
## 15 Oxigenação por membrana extra-corporal, traqueostomia com ventilação mecânica >96h ou traqueostomia com outro diagnóstico principal, excepto da face, boca ou do pescoço
## 19 Oxigenação por membrana extra-corporal, traqueostomia com ventilação mecânica >96h ou traqueostomia com outro diagnóstico principal, excepto da face, boca ou do pescoço
## 23 Oxigenação por membrana extra-corporal, traqueostomia com ventilação mecânica >96h ou traqueostomia com outro diagnóstico principal, excepto da face, boca ou do pescoço
## 25                                                                                      Diagnósticos de traumatismos múltiplos significativos, com CC major não traumáticas
## 29 Oxigenação por membrana extra-corporal, traqueostomia com ventilação mecânica >96h ou traqueostomia com outro diagnóstico principal, excepto da face, boca ou do pescoço
## 33                                                                                                  Outros procedimentos em B.O., por traumatismos múltiplos significativos
## 35                                                                                       Osteomielite, artrite séptica e/ou perturbações do tecido conjuntivo, com CC major
## 38                                                                                                                                                Craniotomia com CC  major
## 45                                                                         Acidente isquémico transitório, oclusões pré-cerebrais, convulsões e/ou cefaleias, com CC  major
## 47                                                                                                                            Linfoma e/ou leucemia não aguda, com CC major
## 48                                                                                        Procedimentos não extensos, em B.O., não relacionados com o diagnóstico principal
##    Codigo.GDH Tipo.GDH Cor.triagem               Motivo.admissao
## 1         483     GDHC     Laranja                        Doença
## 5         483     GDHC     Laranja               Acidente Viação
## 12        530     GDHC     Amarelo                        Doença
## 14        541     GDHM  Programada                          <NA>
## 15        483     GDHC     Amarelo                        Doença
## 19        483     GDHC     Laranja Acidente Viação-Atropelamento
## 23        483     GDHC    Vermelho               Acidente Viação
## 25        794     GDHM    Vermelho               Acidente Viação
## 29        483     GDHC     Laranja                        Doença
## 33        732     GDHC     Laranja                         QUEDA
## 35        561     GDHM     Amarelo                        Doença
## 38        530     GDHC     Laranja                        Doença
## 45        532     GDHM     Amarelo                        Doença
## 47        578     GDHM  Programada                          <NA>
## 48        477     GDHC     Amarelo                        Doença
##    Motivo.triagem idade.int
## 1   Muito urgente        56
## 5   Muito urgente        70
## 12        Urgente        69
## 14     Programada         0
## 15        Urgente        61
## 19  Muito urgente        81
## 23      EMERGENTE        43
## 25      EMERGENTE        64
## 29  Muito urgente        67
## 33  Muito urgente        60
## 35        Urgente        80
## 38  Muito urgente        47
## 45        Urgente        48
## 47     Programada        72
## 48        Urgente        76
```
