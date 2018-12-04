for file in *.pdf
do
stem=$(basename $file .pdf)
pdftoppm $file  $stem.png -png -rx 300 -ry 300
done
