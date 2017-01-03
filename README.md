Crossword grid manipulation library and file format converter.

Install via
````
  opam pin add crosspad git://github.com/martindemello/crosspad.git
````

See src/frontend for examples of library usage.

Executables usage:

xwconvert command line tool:

````
$ xwconvert -f <from-format> -t <to-format> -i <input file> -o <output file>
````

Available formats are displayed via

````
$ xwconvert -l
````

crosspad file viewer (currently acrosslite binary files only):

````
$ crosspad <filename>
````
