# 調査データの読み込み
data01 <- read.csv(file="st_data.csv", header=T, fileEncoding = "UTF-8")
data01 <- data01[-1]
head(data01)

# 調査データの相関行列を求める
cor01 <- cor(data01); cor01

# 調査データの相関行列から、固有値を算出する
eigen01 <- eigen(cor01)$values; eigen01

# スクリープロットを描画する
plot(eigen01, type = "b", main = "Scree Plot", xlab = "Number", ylab = "Eigenvalue")

# 因子数を5とおいて分析をすすめる
> # factanal関数で因子分析を行う(初期解)
fit00 <- factanal(x=data01, factors = 5, rotation = "none")
print(fit00, cutoff=0)
# factanal関数で因子分析を行う(バリマックス解)
fit01 <- factanal(x=data01, factors = 5, rotation = "varimax")
print(fit01, cutoff=0)
# factanal関数で因子分析を行う(プロマックス解)
fit02 <- factanal(x=data01, factors = 5, rotation = "promax")
print(fit02, cutoff=0)

# 因子数を7とおいて分析をすすめる
# factanal関数で因子分析を行う(初期解)
fit00 <- factanal(x=data01, factors = 7, rotation = "none")
print(fit00, cutoff=0)
# factanal関数で因子分析を行う(バリマックス解)
fit01 <- factanal(x=data01, factors = 7, rotation = "varimax")
print(fit01, cutoff=0)
# factanal関数で因子分析を行う(プロマックス解)
fit02 <- factanal(x=data01, factors = 7, rotation = "promax")
print(fit02, cutoff=0)

# メモ：factanal関数では初期解の算出は最尤法になっている。B