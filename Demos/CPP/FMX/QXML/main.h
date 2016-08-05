//---------------------------------------------------------------------------

#ifndef mainH
#define mainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <IdBaseComponent.hpp>
#include <IdComponent.hpp>
#include <IdHTTP.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include "qxml.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TIdHTTP *IdHTTP1;
	TPanel *Panel1;
	TButton *btnParse;
	TListView *ListView1;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall btnParseClick(TObject *Sender);
	void __fastcall ListView1ButtonClick(const TObject *Sender, const TListViewItem *AItem,
          const TListItemSimpleControl *AObject);

private:	// User declarations
	void __fastcall EnumNodes(TQXMLNode *ANode);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
