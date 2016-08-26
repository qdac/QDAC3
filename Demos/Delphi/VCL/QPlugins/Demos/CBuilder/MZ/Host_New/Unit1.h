//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "qplugins_vcl_formsvc.hpp"
#include <Data.DB.hpp>
//---------------------------------------------------------------------------
class TForm_Host : public TForm
{
__published:	// IDE-managed Components
	TPageControl *PageControl1;
	TPanel *Panel1;
	TButton *Button1;
	TButton *Button2;
	TButton *Button3;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
private:	// User declarations
	_di_IQFormService Form;
public:		// User declarations
	__fastcall TForm_Host(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm_Host *Form_Host;
//---------------------------------------------------------------------------
#endif
