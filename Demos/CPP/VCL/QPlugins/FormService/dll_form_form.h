// ---------------------------------------------------------------------------

#ifndef dll_form_formH
#define dll_form_formH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Data.DB.hpp>
#include "qplugins.hpp"
#define CppInterfaceFix \
	ULONG STDMETHODCALLTYPE AddRef(void) 	\
	{                                       \
	return _AddRef();                       \
	}                                       \
	ULONG STDMETHODCALLTYPE Release(void)   \
	{                                       \
	return _Release();                      \
	}

// ---------------------------------------------------------------------------
class TForm3 : public TForm, public IQNotify  {
__published: // IDE-managed Components
	TMemo *Memo1;
	void __fastcall FormDestroy(TObject *Sender);

private: // User declarations
	CppInterfaceFix;
	void __stdcall Notify(const unsigned AId,
		Qplugins_params::_di_IQParams AParams, bool &AFireNext);

public: // User declarations
	__fastcall TForm3(TComponent* Owner);
};

// ---------------------------------------------------------------------------
extern PACKAGE TForm3 *Form3;
// ---------------------------------------------------------------------------
#endif
