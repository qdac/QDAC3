//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dll_static_form.h"
#include "qplugins_vcl_formsvc.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
Label2->Caption="±‡“Î∆˜∞Ê±æ:"+IntToHex(__BCPLUSPLUS__,4);
}
//---------------------------------------------------------------------------
void Register()
{
RegisterFormService(L"/Services/Docks/Forms",L"CB2007Static_Single",__classid(TForm2),false);
RegisterFormService(L"/Services/Docks/Forms",L"CB2007Static_Multi",__classid(TForm2),true);
}

void Unregister()
{
QStringW ANames[2]={
	L"CB2007Static_Single",
	L"CB2007Static_Multi"
	};
UnregisterServices(L"/Services/Docks/Forms",ANames,1);
}

#pragma startup Register
#pragma exit Unregister

