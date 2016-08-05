//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "Unit2.h"
//---------------------------------------------------------------------------
//#pragma package(smart_init)
#pragma resource "*.dfm"
TForm_PathSetup *Form_PathSetup;
//---------------------------------------------------------------------------
__fastcall TForm_PathSetup::TForm_PathSetup(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm_PathSetup::Init(UnicodeString Paths)
{
 LB->Items->StrictDelimiter=true;
 LB->Items->Delimiter=';';
 LB->Items->DelimitedText=Paths;
 for(int i=0;i<LB->Items->Count;i++)
   {
	Paths=LB->Items->Strings[i];
	Paths=Repl(Paths,"\"","");
	LB->Items->Strings[i]=ExcludeTrailingPathDelimiter(Paths);
   }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TForm_PathSetup::GetPaths()
{
 UnicodeString Str,Paths="";
 for(int i=0;i<LB->Items->Count;i++)
   {
	Str=LB->Items->Strings[i];
	Str=Repl(Str,"\"","");
	if(Str!="")
	  {
	   if(Paths!="")
		 Paths+=";";
	   Paths+=ExcludeTrailingPathDelimiter(Str);
	  }
   }
 return Paths;
}
//---------------------------------------------------------------------------
void __fastcall TForm_PathSetup::Button1Click(TObject *Sender)
{
 UnicodeString Directory;
 Directory=FolderBrowser(E1->Text);
 if(Directory!="")
   E1->Text=Directory;
}
//---------------------------------------------------------------------------
void __fastcall TForm_PathSetup::Button2Click(TObject *Sender)
{
 if(E1->Text.Trim()=="")
   return;
 if(LB->Items->IndexOf(E1->Text.Trim())>=0)
   return;
 E1->Text=Repl(E1->Text.Trim(),"\"","");
 LB->Items->Add(ExcludeTrailingPathDelimiter(E1->Text.Trim()));
}
//---------------------------------------------------------------------------
void __fastcall TForm_PathSetup::Button5Click(TObject *Sender)
{
 if(LB->ItemIndex<0||LB->SelCount==0)
   return;
 LB->DeleteSelected();
}
//---------------------------------------------------------------------------
void __fastcall TForm_PathSetup::BtnSaveClick(TObject *Sender)
{
 BtnSave->Tag=1;
 Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm_PathSetup::Button7Click(TObject *Sender)
{
 Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm_PathSetup::Button3Click(TObject *Sender)
{
 if(LB->ItemIndex<0)
   return;
 if(LB->ItemIndex==LB->Items->Count-1)
   return;
 LB->Items->Move(LB->ItemIndex,LB->ItemIndex+1);
}
//---------------------------------------------------------------------------
void __fastcall TForm_PathSetup::Button4Click(TObject *Sender)
{
 if(LB->ItemIndex<1)
   return;
 LB->Items->Move(LB->ItemIndex,LB->ItemIndex-1);
}
//---------------------------------------------------------------------------
