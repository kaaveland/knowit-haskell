Nøtten består i å skrive en funksjon som gitt er matrise returnerer den transponerte
matrisen.

Den transponerte av en matrise er matrisen som har samme kolonne-vektorer som
rad-vektorene i input-matrisen.

Pseudo-kode for en imparativ løsning:

Matrix transpose(Matrix m)
    Matrix transposed = new Matrix()
    transposed.width = m.height
    transposed.height = m.width

    for i = 0 to m.width
        for j = 0 to m.height
            transposed[j][i] = m[i][j]

Ikke benytt funksjoner som ikke finnes i Prelude (ingen imports)

> type Row a = [a]
> type Matrix a = [Row a]

Din kode her:

