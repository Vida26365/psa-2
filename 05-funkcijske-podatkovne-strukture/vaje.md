# Funkcijske podatkovne strukture

## 1. naloga

Utežene levičarske kopice (*weight-biased leftist heaps*) so alternativa levičarskim kopicam, ki za rang namesto dolžine desnega roba vzamejo število elementov.

1. Dokažite, da desni rob utežene levičarske kopice še vedno vsebuje največ $⌊\log(n + 1)⌋$ elementov.

2. Implementacijo v [heap.hs](heap.hs) dopolnite s tipom `WeightBiasedLeftistHeap` in ustrezno implementacijo.

3. Trenutna funkcija `merge` deluje v dveh prehodih: v prvem (od zgoraj navzdol) kliče samo `merge`, v drugem (od spodaj navzgor) pa pomožno funkcijo `makeT`. Pri uteženih levičarskih prehodih je funkcijo `merge` možno implementirati zgolj s prehodom od zgoraj navzdo. Razmislite zakaj, implementirajte to različico in razmislite o njenih prednostih pri lenem izvajanju.
