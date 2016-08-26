// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "qplugins_vcl_formsvc.hpp"
#include "qplugins_vcl_messages.hpp"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm_Frame *Form_Frame;

// ---------------------------------------------------------------------------
__fastcall TForm_Frame::TForm_Frame(TComponent* Owner) : TForm(Owner)
{
}

// ---------------------------------------------------------------------------
void __fastcall TForm_Frame::FormCreate(TObject *Sender)
{
	Edit1->Text = DateTimeToStr(Now());
}
// ---------------------------------------------------------------------------

void Register()
{
	RegisterFormService(L"/Services/Docks/Forms", L"CB2010Static_Single",
		__classid(TForm_Frame), false);
	RegisterFormService(L"/Services/Docks/Forms", L"CB2010Static_Multi",
		__classid(TForm_Frame), true);
}
// ---------------------------------------------------------------------------

void Unregister()
{
	QStringW ANames[2] =
	{
		L"CB2010Static_Single", L"CB2010Static_Multi"
	};
	UnregisterServices(L"/Services/Docks/Forms", ANames, 1);
}

#pragma startup Register
#pragma exit Unregister
// ---------------------------------------------------------------------------
