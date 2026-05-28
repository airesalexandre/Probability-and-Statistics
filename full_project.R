# Projeto Final em R
# Alexandre Aires - ist1110421

# ----------------------------------------------//--------------------------------------------------

# EXERCICIO 4:
# Valor esperado teórico
lambda <- 28
k <- 11
esperado_teorico <- lambda * gamma(1 + 1/k)

# Valor esperado amostral
set.seed(2219)
amostra <- rweibull(7500, shape = k, scale = lambda)
esperado_amostral <- mean(amostra)

# Diferença absoluta
diferenca <- abs(esperado_teorico - esperado_amostral)
round(diferenca, 4)

# ----------------------------------------------//--------------------------------------------------

#EXERCICIO 5:
# Definir a semente
set.seed(2054)

# Número de jogadas
n <- 45000

# Simular lançamentos dos 3 dados para cada jogada
dados <- matrix(sample(1:6, 3 * n, replace = TRUE), ncol = 3)

# Somar os 3 dados por jogada
somas <- rowSums(dados)

# Calcular as frequências relativas
freq_9 <- sum(somas == 9) / n
freq_10 <- sum(somas == 10) / n

# Diferença das frequências relativas
dif <- round(freq_10 - freq_9, 4)

# Mostrar o resultado
dif

# ----------------------------------------------//--------------------------------------------------

#EXERCICIO 6:
# Valor exato usando a CDF da distribuição de Irwin-Hall
irwin_hall_cdf <- function(x, n) {
  if (x < 0) return(0)
  if (x > n) return(1)
  
  k_vals <- 0:floor(x)
  sum_val <- sum((-1)^k_vals * choose(n, k_vals) * (x - k_vals)^n)
  
  return(sum_val / factorial(n))
}

n <- 7
x <- 3.3
p_n <- irwin_hall_cdf(x, n)

# Aproximação com o Teorema do Limite Central (TLC)
mu <- n * 0.5
sigma <- sqrt(n / 12)
p_n_TLC <- pnorm(x, mean = mu, sd = sigma)

# Simulação
set.seed(2486)
m <- 110
samples <- matrix(runif(m * n), nrow = m)
S_n <- rowSums(samples)
p_n_sim <- mean(S_n <= x)

# Desvio absoluto entre p_n e p_n_TLC
desvio_1 <- abs(p_n - p_n_TLC)

# Desvio absoluto entre p_n e p_n_sim
desvio_2 <- abs(p_n - p_n_sim)

# Quociente entre os desvios
quociente <- round(desvio_1 / desvio_2, 4)

# Mostrar resultado final
quociente

# ----------------------------------------------//--------------------------------------------------

#EXERCICIO 7:
# Dados fornecidos
n <- 16
soma_x <- 152.3
soma_logx <- 36

# Média amostral
media <- soma_x / n

# Função a ser igualada a zero (derivada do log-verossimilhança)
log_veross <- function(alpha) {
  log(alpha) - digamma(alpha) - log(media) + (soma_logx / n)
}

# Usar uniroot para encontrar alpha no intervalo dado
alpha_est <- uniroot(log_veross, interval = c(48.6, 153.4))$root

# Estimar lambda com base na média
lambda_est <- alpha_est / media

# Calcular o modo
modo <- (alpha_est - 1) / lambda_est

# Arredondar para 2 casas decimais
round(modo, 2)

# ----------------------------------------------//--------------------------------------------------

# EXERCICIO 8:
set.seed(1746)

# Parâmetros
mu <- -0.4
sigma <- 0.6
n <- 17
m <- 1000
gamma <- 0.91

# Gerar m amostras de tamanho n
amostras <- matrix(rnorm(n * m, mean = mu, sd = sigma), nrow = m, ncol = n)

# Nível z para o IC (normal padrão)
z <- qnorm(1 - (1 - gamma) / 2)

# Médias amostrais
medias <- rowMeans(amostras)

# Margem de erro
erro <- z * sigma / sqrt(n)

# Limites inferior e superior
lim_inf <- medias - erro
lim_sup <- medias + erro

# Verificar se o verdadeiro mu está no intervalo
cobre_mu <- (lim_inf <= mu) & (mu <= lim_sup)

# Proporção que cobre
prop_cobre <- mean(cobre_mu)

# Quociente pedido
resultado <- prop_cobre / gamma

# Mostrar resultado com 4 casas decimais
round(resultado, 4)

# ----------------------------------------------//--------------------------------------------------

#EXERCICIO 9:
# Parâmetros
set.seed(5779)
m <- 900       # número de simulações
n <- 12        # dimensão da amostra
mu0 <- 3       # média sob H0
mu1 <- 3.5     # média sob H1
alpha <- 0.03  # nível de significância

# Limite crítico (quantil 1 - alpha da qui-quadrado com 2n graus de liberdade)
q_crit <- qchisq(1 - alpha, df = 2 * n)

# Estimativa empírica de beta chapéu
rejeicoes <- replicate(m, {
  x <- rexp(n, rate = 1 / mu1)  # gerar amostra sob H1
  T0 <- (2 * n * mean(x)) / mu0
  T0 <= q_crit  # não rejeita H0 → conta para erro de 2ª espécie
})

beta_hat <- mean(rejeicoes)

# Cálculo de beta teórico
# T segue uma qui-quadrado com 2n g.l. multiplicada por mu1/mu0
beta_teorico <- pchisq(q_crit * (mu0 / mu1), df = 2 * n)

# Quociente
quociente <- round(beta_hat / beta_teorico, 4)

# Resultados
cat("Beta chapéu (estimado) = ", round(beta_hat, 4), "\n")
cat("Beta teórico           = ", round(beta_teorico, 4), "\n")
cat("Quociente              = ", quociente, "\n")

# ----------------------------------------------//--------------------------------------------------

#EXERCICIO 10:
# 1. Dados (copiados da imagem e convertidos em vetor numérico)
dados <- c(
  2.8, 7.9, 2.3, 7.1, 1.8, 8.4, 5.8, 5.8, 3.3, 1.8, 4.1, 6, 0.9, 2.9, 4.7, 8.1, 2.1, 1.6,
  5.3, 0.9, 5.7, 2, 7.6, 0.3, 3.7, 2.6, 8.8, 7.1, 5.7, 4.6, 13.1, 3.9, 3.2, 1.7, 5.2, 4.9,
  2.7, 0.6, 1.9, 6, 1.2, 5.2, 4, 5.2, 8.9, 7, 8.7, 5, 4.3, 1.5, 2.4, 7.6, 2.3, 4.4, 4.1,
  1.6, 2.2, 1.7, 3.9, 5.6, 2.9, 5.2, 3.4, 1.7, 8.8, 5.3, 4.9, 5.4, 3, 5.3, 4.5, 3.3, 2.9,
  7.6, 1.7, 7.8, 2.5, 5, 2.1, 5.6, 2.7, 3.5, 5.1, 1.8, 7.2, 3.8, 6.5, 8.4, 5.1, 6.3, 3.2,
  7.3, 4.9, 9.4, 4.7, 4.4, 3.7, 2.5, 2.6, 5.3, 3.7, 2.8, 4.2, 6.5, 6.8, 1.2, 3.2, 7.2, 7.8,
  8.6, 2.1, 7.4, 4.9, 5.3, 5.1, 1.4, 1.4, 2.3, 2.9, 11.3, 4.7, 9.1, 6.9, 5, 1.4, 5.7, 3.9,
  4.9, 7.5, 5.4, 2.3, 3.3, 8, 6.5, 4.4, 2.3, 1.1, 4.5, 3.2, 1.3, 0.8, 6.3, 8, 6.8, 7.6,
  0.6, 2.5, 3.9, 4.8, 4.9, 9.9, 2.7, 6.1, 4.6, 1.2, 5.3, 7, 4.3, 5.2, 1.3, 4.5, 2.5, 4.4,
  1, 3.9, 4.6, 4.4, 3.1, 6.1, 3.9, 6, 6.4, 1.2, 9.2, 2.9, 2.4, 8, 2.7, 2.8, 11.2, 4.1, 5.7,
  2.4, 4.2, 5.9, 1.6, 2.7, 1, 9.2, 9.2, 2.2, 6, 4.6, 2.2, 3.3, 7.5, 0.2, 1.2, 1, 1.5, 3.1,
  6.9, 1.8, 3.2, 0.2, 1.4, 7.2, 0.9, 2.6, 4.5, 5.7, 6.6, 6, 1.7, 6.3, 2.4, 4.3, 1.7, 5.2,
  4.7, 2.8, 2.8, 4.3, 4.5, 3.8, 2.9, 2.2, 5.9, 7.6, 1.7, 3.8, 2.7, 8.5, 2, 3.7, 2.3, 8.6, 1.2, 5.1, 4.7
)

# 2. Subamostra com n = 191 e seed 5443
set.seed(5443)
amostra <- sample(dados, size = 191, replace = FALSE)

# 3. Dividir em 8 classes equiprováveis sob H0: Rayleigh(σ = 3.6)
sigma0 <- 3.6
k <- 8
probs <- seq(0, 1, length.out = k + 1)  # 0, 1/8, 2/8, ..., 1
limites <- sqrt(-2 * sigma0^2 * log(1 - probs[-1]))  # q função inversa de F(x)

# 4. Contar frequências observadas
obs_freq <- table(cut(amostra, breaks = c(0, limites), include.lowest = TRUE))

# 5. Esperadas: todas iguais, pois as classes são equiprováveis
exp_freq <- rep(length(amostra) / k, k)

# 6. Teste do qui-quadrado
teste <- chisq.test(x = obs_freq, p = rep(1 / k, k))

# Resultados
cat("Estatística qui-quadrado =", round(teste$statistic, 4), "\n")
cat("p-valor =", round(teste$p.value, 4), "\n")
