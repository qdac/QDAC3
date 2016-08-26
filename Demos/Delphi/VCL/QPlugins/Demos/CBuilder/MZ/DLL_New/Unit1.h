//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Data.DB.hpp>
//---------------------------------------------------------------------------
class TForm_Frame : public TForm
{
__published:	// IDE-managed Components
	TButton *Button1;
	TPanel *Panel1;
	TEdit *Edit1;
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm_Frame(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm_Frame *Form_Frame;
//---------------------------------------------------------------------------
#endif
