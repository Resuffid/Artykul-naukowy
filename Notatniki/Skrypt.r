library(tidyverse)
library(dplyr)
library(lubridate)
library(gt)
library(tinytex)
library(psych)
library(officer)
library(huxtable)
library(lsr)
library(ggplot2)

wyniki1 <- read_csv("/Users/wiktor/Documents/Studia/Metodologia/GitHub/Artykul/Notatniki/Kwestionariusz.csv")

wyniki <- wyniki1 %>%
  rename(time = `Sygnatura czasowa`,
         approval = `Potwierdzam, że ukończyłam 18 lat i wyrażam zgodę na udział w badaniu`,
         gender = `Proszę podać swoją płeć.`,
         age = `Proszę podać swój wiek w latach.`,
         town = `Proszę podać wielkość miejsca zamieszkania`,
         student = `Czy aktualnie studiujesz/uczysz się?`,
         ig_account = `Czy posiadasz konto na platformie Instagram?`,
         Publikowanie = `Jeśli masz konto na platformie Instagram to czy udostępniasz na niej zdjęcia swojego wizerunku?`,
         P1 = `Szanuję swoje ciało`,
         P2 = `Czuję się dobrze ze swoim ciałem`,
         P3 = `Uważam, że moje ciało ma przynajmniej kilka zalet`,
         P4 = `Mam pozytywny stosunek do swojego ciała`,
         P5 = `Zwracam uwagę na potrzeby swojego ciała`,
         P6 = `Kocham swoje ciało`,
         P7 = `Cenię odmienne i niepowtarzalne cechy swojego ciała`,
         P8 = `Moje zachowanie wyraża mój pozytywny stosunek do mojego ciała; na przykład chodzę z podniesioną głową i uśmiecham się`,
         P9 = `Czuję się komfortowo w swoim ciele`,
         P10 = `Czuję się piękny/-a, nawet jeśli wyglądam inaczej niż atrakcyjne osoby prezentowane w mediach (np. modele/modelki, aktorzy/aktorki)`,
         Modyfikacja = `Czy kiedykolwiek, w jakikolwiek sposób zmodyfikowałaś zdjęcie swojego wizerunku na platformie Instagram tak, aby prezentować się na nim lepiej (prosimy o nieuwzględnianie filtrów, które jedynie zmieniają kolory na zdjęciach)?`,
         truth = `Na wszystkie powyższe pytania odpowiedziałam zgodnie z prawdą i wyrażam zgodę na użycie ich w analizie wyników badania.`)

wyniki <- wyniki %>%
  mutate(pretty_timestamp = ymd_hms(time))

sumy <- wyniki %>%
  mutate(Suma = P1+P2+P3+P4+P5+P6+P7+P8+P9+P10)

filtr <- sumy[!(sumy$gender == "Mężczyzna" | sumy$gender == "Inna"),]
filtr2 <- filtr[!(filtr$ig_account == "Nie"),]
filtr1 <- filtr2[!(filtr2$Modyfikacja == "Nie publikuję swoich zdjęć na platformie Instagram"),]

ost <- filtr1 %>%
  select(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, Suma, Modyfikacja)

ggplot(ost) +
  geom_histogram(aes(x = Suma), binwidth = 5)

ostateczne <- read_csv("/Users/wiktor/Documents/Studia/Metodologia/GitHub/Artykul/Notatniki/Notatnik.csv")

ostateczne %>%
  group_by(Modyfikacja) %>%
  summarise(liczebnosc = n(),
            M = mean(Suma),
            SD = sd(Suma),
            min = min(Suma),
            max = max(Suma),
            Mediana = median(Suma),
            Rozstęp = max(Suma)-min(Suma)
  )

t_suma <- t.test(Suma ~ Modyfikacja, data = ostateczne, subset = Modyfikacja == "Tak" | Modyfikacja == "Nie")
t_suma

shapiro.test(ostateczne$Suma)

cohensD(Suma ~ Modyfikacja, data = ostateczne)

ostateczne <- ostateczne %>%
  mutate(Modyfikacja = as.factor(Modyfikacja))
cohen.d(ostateczne$Suma, ostateczne$Modyfikacja)

ggplot(ostateczne) +
  geom_boxplot(aes(x=Modyfikacja, y=Suma)) +
  xlab("Modyfikacja zdjęć") +
  ylab("Wynik zadowolenia z ciała") +
  theme_classic()

