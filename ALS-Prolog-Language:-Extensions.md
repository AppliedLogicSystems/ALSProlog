Dummy Page with example table:

First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell


Compare editing the following Markdown and HTML tables:

Expr | Octal Expr | Hex Expr | ASCI Code | Char
:---:|:----------:|:--------:|:---------:|:---------------:
0’A  | 0’\101     | 0’\x41   | 65        | Upper case A
0’c  | 0’\143     | 0’\x63   | 99        | Lower case c
0’~  | 0’\176     | 0’\x7e   | 126       | Tilde character

<table>
<tr><th>Expr</th><th>Octal Expr</th><th>Hex Expr</th><th>ASCI Code</th><th>Char</th></tr>
<tr><td>0’A</td><td>0’\101</td><td>0’\x41</td><td>65</td><td>Upper case A</td></tr>
<tr><td>0’c</td><td>0’\143/td><td>0’\x63/td><td>99/td><td>Lower case c</td></tr>
<tr><td>0’~</td><td>0’\176</td><td>0’\x7e</td><td>126</td><td>Tilde character</td></tr>
</table>