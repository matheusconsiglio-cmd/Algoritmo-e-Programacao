Algoritmo "LogiFast"

Const
   MAX_ENTREGAS = 30
   MAX_REGIOES = 5
   MAX_PERCURSO = 30
   URGENTE = 1
   PRIORITARIA = 2
   NORMAL = 3
   AGUARDANDO = 1
   EM_TRANSPORTE = 2
   ENTREGUE = 3
   VALOR_KM = 2.50
   PESO_LEVE = 5
   PESO_MEDIO = 20
   PESO_PESADO = 50
   TAXA_PESO_MEDIO = 0.10
   TAXA_PESO_PESADO = 0.20
   TAXA_PESO_EXTRA = 0.35
   TAXA_PRIORITARIA = 0.15
   TAXA_URGENTE = 0.30

Tipo
   Entrega = registro
      codigo: inteiro
      destinatario: caractere
      origem: inteiro
      destino: inteiro
      peso: real
      valorMercadoria: real
      prioridade: inteiro
      distancia: inteiro
      status: inteiro
      custo: real
   fimregistro

Var
   opcao: inteiro
   distancias: vetor[1..MAX_REGIOES, 1..MAX_REGIOES] de inteiro
   entregas: vetor[1..MAX_ENTREGAS] de Entrega
   quantidadeEntregas: inteiro
   percurso: vetor[1..MAX_PERCURSO] de inteiro

Procedimento inicializarDistancias
Inicio
   distancias[1, 1] <- 0
   distancias[1, 2] <- 12
   distancias[1, 3] <- 15
   distancias[1, 4] <- 10
   distancias[1, 5] <- 14

   distancias[2, 1] <- 12
   distancias[2, 2] <- 0
   distancias[2, 3] <- 25
   distancias[2, 4] <- 18
   distancias[2, 5] <- 20

   distancias[3, 1] <- 15
   distancias[3, 2] <- 25
   distancias[3, 3] <- 0
   distancias[3, 4] <- 16
   distancias[3, 5] <- 19

   distancias[4, 1] <- 10
   distancias[4, 2] <- 18
   distancias[4, 3] <- 16
   distancias[4, 4] <- 0
   distancias[4, 5] <- 22

   distancias[5, 1] <- 14
   distancias[5, 2] <- 20
   distancias[5, 3] <- 19
   distancias[5, 4] <- 22
   distancias[5, 5] <- 0
Fimprocedimento

Procedimento exibirMatriz
Var
   linha, coluna: inteiro
   maiorDistancia: inteiro
   menorDistancia: inteiro
   somaDistancias, quantidadeDistancias: inteiro
   mediaDistancias: real
Inicio
   menorDistancia <- distancias[1, 2]
   maiorDistancia <- 0
   somaDistancias <- 0
   quantidadeDistancias <- 0

   escreval("DISTANCIAS EM KM")
   escreval("            Centro   Norte     Sul   Leste   Oeste")

   para linha de 1 ate MAX_REGIOES faca
      escolha linha
         caso 1
            escreva("Centro  ")
         caso 2
            escreva("Norte   ")
         caso 3
            escreva("Sul     ")
         caso 4
            escreva("Leste   ")
         caso 5
            escreva("Oeste   ")
      fimescolha

      para coluna de 1 ate MAX_REGIOES faca
         escreva(distancias[linha, coluna]:8)

         se distancias[linha, coluna] > maiorDistancia entao
            maiorDistancia <- distancias[linha, coluna]
         fimse
         se (distancias[linha, coluna] <> 0) e (distancias[linha, coluna] < menorDistancia) entao
            menorDistancia <- distancias[linha, coluna]
         fimse
         se linha <> coluna entao
            somaDistancias <- somaDistancias + distancias[linha, coluna]
            quantidadeDistancias <- quantidadeDistancias + 1
         fimse
      fimpara

      escreval
   fimpara

   mediaDistancias <- somaDistancias / quantidadeDistancias

   escreval
   escreval("Maior distancia: ", maiorDistancia, " km")
   escreval("Menor distancia entre regioes: ", menorDistancia, " km")
   escreval("Media das distancias: ", mediaDistancias:6:2, " km")
Fimprocedimento

Funcao calcularCusto(distancia: inteiro; peso: real; prioridade: inteiro): real
Var
   custoBase: real
   acrescimoPeso, acrescimoPrioridade: real
Inicio
   custoBase <- distancia * VALOR_KM
   acrescimoPeso <- 0
   acrescimoPrioridade <- 0

   se peso <= PESO_LEVE entao
      acrescimoPeso <- 0
   senao
      se peso <= PESO_MEDIO entao
         acrescimoPeso <- TAXA_PESO_MEDIO
      senao
         se peso <= PESO_PESADO entao
            acrescimoPeso <- TAXA_PESO_PESADO
         senao
            acrescimoPeso <- TAXA_PESO_EXTRA
         fimse
      fimse
   fimse

   escolha prioridade
      caso URGENTE
         acrescimoPrioridade <- TAXA_URGENTE
      caso PRIORITARIA
         acrescimoPrioridade <- TAXA_PRIORITARIA
      caso NORMAL
         acrescimoPrioridade <- 0
   fimescolha

   retorne custoBase * (1 + acrescimoPeso + acrescimoPrioridade)
Fimfuncao

Funcao localizarEntrega(codigoBuscado: inteiro; quantidade: inteiro): inteiro
Var
   i, posicaoEncontrada: inteiro
Inicio
   posicaoEncontrada <- 0
   i <- 1

   enquanto (i <= quantidade) e (posicaoEncontrada = 0) faca
      se entregas[i].codigo = codigoBuscado entao
         posicaoEncontrada <- i
      fimse
      i <- i + 1
   fimenquanto

   retorne posicaoEncontrada
Fimfuncao

Procedimento cadastrarEntrega
Var
   nova: Entrega
   posicao: inteiro
Inicio
   se quantidadeEntregas >= MAX_ENTREGAS entao
      escreval("Limite de entregas atingido.")
   senao
      repita
         escreva("Codigo da entrega: ")
         leia(nova.codigo)

         posicao <- localizarEntrega(nova.codigo, quantidadeEntregas)
         se posicao <> 0 entao
            escreval("Codigo ja cadastrado. Digite outro.")
         fimse
      ate posicao = 0

      escreva("Nome do destinatario: ")
      leia(nova.destinatario)

      escreval("REGIOES")
      escreval("1 - Centro")
      escreval("2 - Norte")
      escreval("3 - Sul")
      escreval("4 - Leste")
      escreval("5 - Oeste")

      repita
         escreva("Regiao de origem: ")
         leia(nova.origem)

         se (nova.origem < 1) ou (nova.origem > MAX_REGIOES) entao
            escreval("Origem invalida. Digite de 1 a 5.")
         fimse
      ate (nova.origem >= 1) e (nova.origem <= MAX_REGIOES)

      repita
         escreva("Regiao de destino: ")
         leia(nova.destino)

         se (nova.destino < 1) ou (nova.destino > MAX_REGIOES) entao
            escreval("Destino invalido. Digite de 1 a 5.")
         senao
            se nova.destino = nova.origem entao
               escreval("Destino deve ser diferente da origem.")
            fimse
         fimse
      ate (nova.destino >= 1) e (nova.destino <= MAX_REGIOES) e (nova.destino <> nova.origem)

      repita
         escreva("Peso em kg: ")
         leia(nova.peso)

         se nova.peso <= 0 entao
            escreval("O peso deve ser maior que zero.")
         fimse
      ate nova.peso > 0

      repita
         escreva("Valor da mercadoria: R$ ")
         leia(nova.valorMercadoria)

         se nova.valorMercadoria < 0 entao
            escreval("O valor nao pode ser negativo.")
         fimse
      ate nova.valorMercadoria >= 0

      escreval("PRIORIDADES")
      escreval("1 - Urgente")
      escreval("2 - Prioritaria")
      escreval("3 - Normal")

      repita
         escreva("Prioridade: ")
         leia(nova.prioridade)

         se (nova.prioridade < URGENTE) ou (nova.prioridade > NORMAL) entao
            escreval("Prioridade invalida. Digite de 1 a 3.")
         fimse
      ate (nova.prioridade >= URGENTE) e (nova.prioridade <= NORMAL)

      nova.status <- AGUARDANDO
      nova.distancia <- distancias[nova.origem, nova.destino]
      nova.custo <- calcularCusto(nova.distancia, nova.peso, nova.prioridade)

      quantidadeEntregas <- quantidadeEntregas + 1
      entregas[quantidadeEntregas] <- nova

      escreval("Entrega cadastrada!")
      escreval("Status: Aguardando")
      escreval("Distancia: ", nova.distancia, " km")
      escreval("Custo estimado: R$ ", nova.custo:8:2)
   fimse
Fimprocedimento

Procedimento mostrarEntrega(posicao: inteiro)
Inicio
   escreval("----------------------------------------")
   escreval("Codigo: ", entregas[posicao].codigo)
   escreval("Destinatario: ", entregas[posicao].destinatario)
   escreval("Origem: ", entregas[posicao].origem)
   escreval("Destino: ", entregas[posicao].destino)
   escreval("Peso: ", entregas[posicao].peso:8:2, " kg")
   escreval("Valor da mercadoria: R$ ", entregas[posicao].valorMercadoria:8:2)
   escreval("Distancia: ", entregas[posicao].distancia, " km")
   escreval("Custo estimado: R$ ", entregas[posicao].custo:8:2)

   escolha entregas[posicao].prioridade
      caso URGENTE
         escreval("Prioridade: Urgente")
      caso PRIORITARIA
         escreval("Prioridade: Prioritaria")
      caso NORMAL
         escreval("Prioridade: Normal")
   fimescolha

   escolha entregas[posicao].status
      caso AGUARDANDO
         escreval("Status: Aguardando")
      caso EM_TRANSPORTE
         escreval("Status: Em transporte")
      caso ENTREGUE
         escreval("Status: Entregue")
   fimescolha
Fimprocedimento

Procedimento listarEntregas
Var
   i: inteiro
Inicio
   se quantidadeEntregas = 0 entao
      escreval("Nenhuma entrega cadastrada.")
   senao
      escreval("REGIOES: 1-Centro, 2-Norte, 3-Sul, 4-Leste, 5-Oeste")

      para i de 1 ate quantidadeEntregas faca
         mostrarEntrega(i)
      fimpara
   fimse
Fimprocedimento

Procedimento consultarEntrega
Var
   codigoBuscado, posicao: inteiro
Inicio
   escreva("Codigo da entrega: ")
   leia(codigoBuscado)

   posicao <- localizarEntrega(codigoBuscado, quantidadeEntregas)

   se posicao = 0 entao
      escreval("Entrega nao encontrada.")
   senao
      escreval("REGIOES: 1-Centro, 2-Norte, 3-Sul, 4-Leste, 5-Oeste")
      mostrarEntrega(posicao)
   fimse
Fimprocedimento

Procedimento alterarStatus(statusNecessario, novoStatus: inteiro)
Var
   codigoBuscado, posicao: inteiro
Inicio
   escreva("Codigo da entrega: ")
   leia(codigoBuscado)

   posicao <- localizarEntrega(codigoBuscado, quantidadeEntregas)

   se posicao = 0 entao
      escreval("Entrega nao encontrada.")
   senao
      se entregas[posicao].status = statusNecessario entao
         entregas[posicao].status <- novoStatus

         se novoStatus = EM_TRANSPORTE entao
            escreval("Transporte iniciado.")
         senao
            escreval("Entrega finalizada.")
         fimse
      senao
         escolha entregas[posicao].status
            caso AGUARDANDO
               escreval("A entrega ainda esta aguardando.")
               escreval("Inicie o transporte antes de finalizar.")
            caso EM_TRANSPORTE
               escreval("A entrega ja esta em transporte.")
            caso ENTREGUE
               escreval("A entrega ja foi concluida e nao pode ser alterada.")
         fimescolha
      fimse
   fimse
Fimprocedimento

// Compara duas entregas, respeitando a ordem dos quatro criterios.
Funcao vemAntes(a, b: inteiro): logico
Inicio
   se entregas[a].prioridade <> entregas[b].prioridade entao
      retorne entregas[a].prioridade < entregas[b].prioridade
   fimse
   se entregas[a].valorMercadoria <> entregas[b].valorMercadoria entao
      retorne entregas[a].valorMercadoria > entregas[b].valorMercadoria
   fimse
   se entregas[a].peso <> entregas[b].peso entao
      retorne entregas[a].peso > entregas[b].peso
   fimse
   retorne entregas[a].codigo < entregas[b].codigo
Fimfuncao

Procedimento exibirFila
Var
   i, j, primeiro, quantidadeFila: inteiro
   auxiliar: Entrega
Inicio
   // Selection Sort: procura o primeiro da parte ainda nao ordenada.
   se quantidadeEntregas > 1 entao
      para i de 1 ate quantidadeEntregas - 1 faca
         primeiro <- i
         para j de i + 1 ate quantidadeEntregas faca
            se vemAntes(j, primeiro) entao
               primeiro <- j
            fimse
         fimpara
         se primeiro <> i entao
            auxiliar <- entregas[i]
            entregas[i] <- entregas[primeiro]
            entregas[primeiro] <- auxiliar
         fimse
      fimpara
   fimse

   quantidadeFila <- 0
   escreval("FILA DE DESPACHO")
   se quantidadeEntregas > 0 entao
      para i de 1 ate quantidadeEntregas faca
         se entregas[i].status = AGUARDANDO entao
            quantidadeFila <- quantidadeFila + 1
            escreval("Posicao na fila: ", quantidadeFila)
            mostrarEntrega(i)
         fimse
      fimpara
   fimse
   se quantidadeFila = 0 entao
      escreval("Nenhuma entrega aguardando transporte.")
   fimse
Fimprocedimento

Procedimento exibirEstatisticas(quantidade: inteiro)
Var
   i, regiaoMaisEntregas: inteiro
   porStatus: vetor[1..3] de inteiro
   porPrioridade: vetor[1..3] de inteiro
   porRegiao: vetor[1..MAX_REGIOES] de inteiro
   maiorValor, maiorPeso, maiorDistancia: inteiro
   pesoTotal, pesoTransportado, valorTotal, custoTotal: real
Inicio
   escreval("Total de entregas: ", quantidade)
   se quantidade = 0 entao
      escreval("Nao ha entregas para calcular as estatisticas.")
   senao
      pesoTotal <- 0
      pesoTransportado <- 0
      valorTotal <- 0
      custoTotal <- 0
      maiorValor <- 1
      maiorPeso <- 1
      maiorDistancia <- 1
      regiaoMaisEntregas <- 1
      para i de 1 ate 3 faca
         porStatus[i] <- 0
         porPrioridade[i] <- 0
      fimpara
      para i de 1 ate MAX_REGIOES faca
         porRegiao[i] <- 0
      fimpara

      para i de 1 ate quantidade faca
         porStatus[entregas[i].status] <- porStatus[entregas[i].status] + 1
         porPrioridade[entregas[i].prioridade] <- porPrioridade[entregas[i].prioridade] + 1
         // Conta por destino, considerando todas as entregas cadastradas.
         porRegiao[entregas[i].destino] <- porRegiao[entregas[i].destino] + 1
         pesoTotal <- pesoTotal + entregas[i].peso
         se entregas[i].status <> AGUARDANDO entao
            pesoTransportado <- pesoTransportado + entregas[i].peso
         fimse
         valorTotal <- valorTotal + entregas[i].valorMercadoria
         custoTotal <- custoTotal + entregas[i].custo

         se entregas[i].valorMercadoria > entregas[maiorValor].valorMercadoria entao
            maiorValor <- i
         fimse
         se entregas[i].peso > entregas[maiorPeso].peso entao
            maiorPeso <- i
         fimse
         se entregas[i].distancia > entregas[maiorDistancia].distancia entao
            maiorDistancia <- i
         fimse
      fimpara

      para i de 2 ate MAX_REGIOES faca
         se porRegiao[i] > porRegiao[regiaoMaisEntregas] entao
            regiaoMaisEntregas <- i
         fimse
      fimpara

      escreval("Aguardando: ", porStatus[AGUARDANDO])
      escreval("Em transporte: ", porStatus[EM_TRANSPORTE])
      escreval("Entregues: ", porStatus[ENTREGUE])
      escreval("Urgentes: ", porPrioridade[URGENTE])
      escreval("Prioritarias: ", porPrioridade[PRIORITARIA])
      escreval("Normais: ", porPrioridade[NORMAL])
      escreval("Peso total transportado (em transporte ou entregue): ", pesoTransportado:8:2, " kg")
      escreval("Peso total das cargas cadastradas: ", pesoTotal:8:2, " kg")
      escreval("Media de peso: ", (pesoTotal / quantidade):8:2, " kg")
      escreval("Valor total das mercadorias: R$ ", valorTotal:8:2)
      escreval("Valor medio das mercadorias: R$ ", (valorTotal / quantidade):8:2)
      escreval("Custo total estimado: R$ ", custoTotal:8:2)
      escreval("Custo medio: R$ ", (custoTotal / quantidade):8:2)
      escreval("Entrega de maior valor: codigo ", entregas[maiorValor].codigo,
               " - R$ ", entregas[maiorValor].valorMercadoria:8:2)
      escreval("Entrega de maior peso: codigo ", entregas[maiorPeso].codigo,
               " - ", entregas[maiorPeso].peso:8:2, " kg")
      escreval("Entrega de maior distancia: codigo ", entregas[maiorDistancia].codigo,
               " - ", entregas[maiorDistancia].distancia, " km")
      escreval("REGIOES: 1-Centro, 2-Norte, 3-Sul, 4-Leste, 5-Oeste")
      escreval("Regiao com mais entregas destinadas: ", regiaoMaisEntregas,
               " - Quantidade: ", porRegiao[regiaoMaisEntregas])
      escreval("Em empates de regiao, foi escolhido o menor codigo regional.")
   fimse
Fimprocedimento

Funcao calcularPercurso(posicao, quantidade: inteiro): inteiro
Inicio
   // Na ultima regiao nao existe um proximo trecho para somar.
   se posicao >= quantidade entao
      retorne 0
   senao
      retorne distancias[percurso[posicao], percurso[posicao + 1]] + calcularPercurso(posicao + 1, quantidade)
   fimse
Fimfuncao

Procedimento lerPercurso
Var
   i, quantidade: inteiro
Inicio
   repita
      escreva("Quantidade de regioes no percurso (2 a 30): ")
      leia(quantidade)
      se (quantidade < 2) ou (quantidade > MAX_PERCURSO) entao
         escreval("Quantidade invalida.")
      fimse
   ate (quantidade >= 2) e (quantidade <= MAX_PERCURSO)

   escreval("REGIOES: 1-Centro, 2-Norte, 3-Sul, 4-Leste, 5-Oeste")
   para i de 1 ate quantidade faca
      repita
         escreva("Codigo da regiao na posicao ", i, ": ")
         leia(percurso[i])
         se (percurso[i] < 1) ou (percurso[i] > MAX_REGIOES) entao
            escreval("Regiao invalida.")
         fimse
      ate (percurso[i] >= 1) e (percurso[i] <= MAX_REGIOES)
   fimpara
   escreval("Distancia total do percurso: ", calcularPercurso(1, quantidade), " km")
Fimprocedimento

Inicio
   inicializarDistancias
   quantidadeEntregas <- 0
   repita
      escreval("")
      escreval("LOGIFAST - SISTEMA DE GERENCIAMENTO DE ENTREGAS")
      escreval("1 - Cadastrar entrega")
      escreval("2 - Listar entregas")
      escreval("3 - Consultar entrega")
      escreval("4 - Exibir fila de despacho")
      escreval("5 - Iniciar transporte")
      escreval("6 - Finalizar entrega")
      escreval("7 - Exibir matriz de distancias")
      escreval("8 - Exibir estatisticas")
      escreval("9 - Calcular percurso recursivamente")
      escreval("10 - Encerrar sistema")
      escreva("Escolha uma opcao: ")
      leia(opcao)

      escolha opcao
         caso 1
            cadastrarEntrega
         caso 2
            listarEntregas
         caso 3
            consultarEntrega
         caso 4
            exibirFila
         caso 5
            alterarStatus(AGUARDANDO, EM_TRANSPORTE)
         caso 6
            alterarStatus(EM_TRANSPORTE, ENTREGUE)
         caso 7
            exibirMatriz
         caso 8
            exibirEstatisticas(quantidadeEntregas)
         caso 9
            lerPercurso
         caso 10
            escreval("Sistema encerrado.")
         outrocaso
            escreval("Opcao invalida.")
      fimescolha

   ate opcao = 10
Fimalgoritmo
