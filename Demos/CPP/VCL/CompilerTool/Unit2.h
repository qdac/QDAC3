//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
//---------------------------------------------------------------------------
class TForm_PathSetup : public TForm
{
__published:	// IDE-managed Components
	TListBox *LB;
	TButton *Button3;
	TButton *Button4;
	TLabel *Label14;
	TEdit *E1;
	TButton *Button1;
	TButton *Button2;
	TButton *Button5;
	TButton *BtnSave;
	TButton *Button7;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall BtnSaveClick(TObject *Sender);
	void __fastcall Button7Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm_PathSetup(TComponent* Owner);
	void __fastcall Init(UnicodeString Paths);
	UnicodeString __fastcall GetPaths();
};
//---------------------------------------------------------------------------
extern TForm_PathSetup *Form_PathSetup;
//---------------------------------------------------------------------------
#endif
