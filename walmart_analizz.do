clear // Mevcut veriyi temizler
cls // Konsol ekranını temizler
set more off // Çıktıların kesintisiz akmasını sağlar

// CSV dosyasını yükleme
import delimited "C:\Users\Monster TR\Desktop\walmart\Walmart_Sales.csv", clear

// Veri setini kontrol etme
describe // Değişkenlerin adları ve türleri
summarize // Sayısal değişkenlerin özet istatistikleri
list // Veri setindeki ilk birkaç satırı görüntüleme


drop if missing(weekly_sales, temperature, fuel_price, cpı, unemployment)
describe
corr

predict residuals, resid //model varsayımlara uygun mu değil mi ona bakılır
sktest residuals


//Hipotez2: Yakıt fiyatlarındaki değişiklikler haftalık satışları anlamlı şekilde etkilemektedir.
//Null Hipotez (H0): Haftalık satışlar ile yakıt fiyatları arasında ilişki yoktur (𝛽 = 0)
//Alternatif Hipotez (H1): Haftalık satışlar ile yakıt fiyatları arasında anlamlı bir ilişki vardır (𝛽 ≠ 0)

regress weekly_sales fuel_price

*Hipotez1: Haftalık satışlar, CPI (Tüketici Fiyat Endeksi) ve işsizlik oranı gibi makroekonomik göstergelerle ilişkilidir.
*Null Hipotez (H0): Haftalık satışlar ile CPI ve işsizlik oranı arasında ilişki yoktur (𝛽 = 0).
*Alternatif Hipotez (H1): Haftalık satışlar ile CPI ve işsizlik oranı arasında anlamlı bir ilişki vardır (𝛽 ≠ 0).

regress weekly_sales cpı unemployment


*Hipotez3: Satış Performansı Yüksek Olan Mağazalar, Satış Performansı Düşük Olan Mağazalardan Demografik Faktörler Açısından Anlamlı Şekilde Farklıdır
*Null Hipotezi (H₀):
*Satış performansı yüksek olan mağazalar ile satış performansı düşük olan mağazalar arasında demografik faktörler açısından istatistiksel olarak anlamlı bir fark yoktur.
*Alternatif Hipotez (H₁):
*Satış performansı yüksek olan mağazalar ile satış performansı düşük olan mağazalar arasında demografik faktörler açısından istatistiksel olarak anlamlı bir fark vardır.

egen total_sales = total(weekly_sales), by(store)   // Mağaza bazında toplam satışlar
egen sales_mean = mean(total_sales)                 // Satışların ortalamasını hesapla
gen sales_group = total_sales > sales_mean          // Ortalama üzerinde: 1 (yüksek), altında: 0 (düşük)

ttest fuel_price, by(sales_group)


*Hipotez4:
*Null Hipotezi (H₀): Sıcaklık, haftalık satışları anlamlı şekilde etkilemez.
*Alternatif Hipotezi (H₁): Sıcaklık, haftalık satışları anlamlı şekilde etkiler.
regress weekly_sales temperature

