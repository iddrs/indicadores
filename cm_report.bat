@echo off

SET /P ano=Informe o ano [AAAA]
SET /P mes=Informe o mês [MM]

RScript.exe cm_generator.R

pdflatex -file-line-error -halt-on-error -output-directory output/ -output-format pdf cm_report.tex

SET filename="indicadores cm %ano%-%mes%.pdf"
echo %filename%

cd output

MOVE /Y cm_report.pdf %filename%
cd ..

pause
exit
