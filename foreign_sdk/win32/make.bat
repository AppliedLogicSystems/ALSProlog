rmdir ALS_Prolog_Foreign_SDK /S

mkdir "ALS_Prolog_Foreign_SDK"
cd "ALS_Prolog_Foreign_SDK"
mkdir ALS_Prolog_Support
copy ..\alspro.dll.lib ALS_Prolog_Support
copy ..\..\source\alspi.h ALS_Prolog_Support
copy ..\..\source\alspi_slib.h ALS_Prolog_Support
copy ..\..\source\alspi_slib.c ALS_Prolog_Support
mkdir Documentation
copy ..\..\docs\FI_Manual.pdf "Documentation\Foreign Interface Manual.pdf"
copy ..\..\docs\FI_Ref.pdf "Documentation\Foreign Interface Reference.pdf"
mkdir Examples
copy ..\..\examples\win32\Examples_Read_Me.txt "Examples\Examples Read Me.txt"
cd Examples
mkdir Even_Example
copy ..\..\..\examples\common\even.c Even_Example
copy ..\..\..\examples\win32\even.mak Even_Example
copy ..\..\..\examples\win32\even.vcp Even_Example
copy ..\..\..\examples\win32\even_x86.mcp Even_Example
mkdir QA_Example
copy ..\..\..\examples\common\qa.c QA_Example
copy ..\..\..\examples\common\know.pro QA_Example
copy ..\..\..\examples\win32\Q_and_A_x86.mcp QA_Example
copy ..\..\..\examples\win32\qa.mak QA_Example
mkdir Tutorial_Example
copy ..\..\..\examples\common\examples.c Tutorial_Example
copy ..\..\..\examples\win32\examples_x86.mcp Tutorial_Example
copy ..\..\..\examples\win32\examples.mak Tutorial_Example
cd ..
cd ..