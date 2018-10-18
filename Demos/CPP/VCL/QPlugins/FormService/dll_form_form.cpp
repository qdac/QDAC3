// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dll_form_form.h"
#include "qplugins_vcl_formsvc.hpp"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm3 *Form3;

// ---------------------------------------------------------------------------
__fastcall TForm3::TForm3(TComponent* Owner) : TForm(Owner) {
	_di_IQNotifyManager AMgr = ToInterface<IQNotifyManager>(PluginsManager());
	if (AMgr) {
		AMgr->Subscribe(AMgr->IdByName(L"CPPNotify"),
			ToInterface<IQNotify>((TObject *)this));
        ScaleForPPI
	}
}

// ---------------------------------------------------------------------------
void __stdcall TForm3::Notify(const unsigned AId,
	Qplugins_params::_di_IQParams AParams, bool &AFireNext) {
	Memo1->Lines->Add(L"Notify Fired");
}

void Register() {
	RegisterFormService(L"/Services/Docks/Forms", L"SingleInstanceForm",
		__classid(TForm3), false);
	RegisterFormService(L"/Services/Docks/Forms", L"MultiInstanceForm",
		__classid(TForm3), true);
}

void Unregister() {
	QStringW ANames[2] = {L"MultiInstanceForm", L"SingleInstanceForm"};
	UnregisterServices(L"/Services/Docks/Forms", ANames, 1);
}

#pragma startup Register
#pragma exit Unregister

void __fastcall TForm3::FormDestroy(TObject *Sender) {
	_di_IQNotifyManager AMgr = ToInterface<IQNotifyManager>(PluginsManager());
	if (AMgr) {
		AMgr->Unsubscribe(AMgr->IdByName(L"CPPNotify"),
			ToInterface<IQNotify>((TObject *)this));
	}
}
// ---------------------------------------------------------------------------
