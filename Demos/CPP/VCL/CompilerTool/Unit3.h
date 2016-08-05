//---------------------------------------------------------------------------

#ifndef Unit3H
#define Unit3H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "qjson.hpp"
#include <Vcl.Grids.hpp>
#include <Vcl.ValEdit.hpp>
#include <Vcl.ComCtrls.hpp>
//---------------------------------------------------------------------------
class TFrame_Compiler : public TFrame
{
__published:	// IDE-managed Components
	TLabel *L;
	TPageControl *PC;
	TTabSheet *TabInstall;
	TLabeledEdit *RootDir;
	TLabeledEdit *App;
	TLabeledEdit *BrowsingPath;
	TTabSheet *TabEnv;
	TValueListEditor *Env;
private:	// User declarations
public:		// User declarations
	__fastcall TFrame_Compiler(TComponent* Owner);
	void __fastcall Init();
	TQJson *QJson;
};
//---------------------------------------------------------------------------
extern TFrame_Compiler *Frame_Compiler;
//---------------------------------------------------------------------------
#endif
