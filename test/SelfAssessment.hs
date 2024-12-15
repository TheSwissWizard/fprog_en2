module SelfAssessment where

-- In diesem Modul beurteilen Sie Ihre eigene Arbeit.
-- 
-- Im ersten Teil beurteilen Sie, ob Sie die einzelnen Kriterien erfüllt haben.
-- Im zweiten Teil geht es um eine Einschätzung der eigenen Leistung in Bezug auf AI Tools.


-- Teil 1: Beurteilung der Erfüllung der Kriterien
-- 
-- Für jedes Kriterium müssen Sie eintragen, ob Ihre Lösung das Kriterium **vollständig** erfüllt
-- oder nicht. Das wird von fünf weiteren Studierenden überprüft und kann in die Note einfliessen.

data Beurteilung = VollständigErfüllt 
                 | NichtVollständigErfüllt 
                 deriving (Show, Eq)

f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, nf1, nf2 :: Beurteilung
-- Funktionale Anforderungen:
f1 = VollständigErfüllt
f2 = VollständigErfüllt
f3 = VollständigErfüllt
f4 = VollständigErfüllt
f5 = VollständigErfüllt
f6 = VollständigErfüllt
f7 = VollständigErfüllt
f8 = VollständigErfüllt
f9 = VollständigErfüllt
f10 = VollständigErfüllt
f11 = VollständigErfüllt

-- Nicht-funktionale Anforderungen:
nf1 = VollständigErfüllt
nf2 = VollständigErfüllt


-- Teil 2: Deklaration zum Verständnis der eigenen Lösung
-- 
-- AI Tools bieten grosse Unterstützung beim Erstellen von Software.
-- Diese neuen Werkzeuge wollen wir nutzen! Allerdings ist es absolut kritisch,
-- dass wir generierten Code nicht einfach übernehmen, sondern im Detail verstehen wie er funktioniert.
--
-- Wenn Sie den Code Ihrer Lösung verstehen und erklären können wie er im Detail funktioniert, 
-- können Sie dies mit `ImDetailUndKannEsErklären` angeben. 
-- Wenn Sie den Code Ihrer Abgabe aber nicht vollständig verstehen, weil gewisse Teile mit Hilfe von AI Tools 
-- generiert wurden, dann müssen Sie das mit `NichtImDetailWeilTeileDavonAiGeneriert` beantworten.
--
-- Konsequenzen der Auswahl:
-- Bei der Deklaration von `NichtImDetailWeilTeileDavonAiGeneriert` wird das Kriterium `S0` mit 0Pt bewertet. Es gibt keine weiteren Konsequenzen.
-- Wenn Sie `ImDetailUndKannEsErklären` deklarieren, gibt das Kriterium `S0` 4Pt. 
-- Bei der EN3 Prüfung werde ich ausgewählten Studierenden Fragen zu ihrer Lösung stellen.
-- Wenn Sie diese dann nicht überzeugend beantworten können, gibt es rückwirkend 2 Noten Abzug für die Projektarbeit.


data Verständnis = ImDetailUndKannEsErklären
                 | NichtImDetailWeilTeileDavonAiGeneriert
                 deriving (Show, Eq)

-- Deklaration zum Verständnis der eigenen Lösung
ichVersteheAlleCodeFragmenteDieserArbeit :: Verständnis
ichVersteheAlleCodeFragmenteDieserArbeit = ImDetailUndKannEsErklären
