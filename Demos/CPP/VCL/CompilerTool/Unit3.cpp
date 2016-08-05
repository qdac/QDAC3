//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit3.h"
//---------------------------------------------------------------------------
//#pragma package(smart_init)
#pragma resource "*.dfm"
TFrame_Compiler *Frame_Compiler;
//---------------------------------------------------------------------------
__fastcall TFrame_Compiler::TFrame_Compiler(TComponent* Owner)
	: TFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFrame_Compiler::Init()
{
 L->Caption=QJson->ForcePath("Name")->AsString;
 RootDir->Text=QJson->ForcePath("RootDir")->AsString;
 App->Text=QJson->ForcePath("App")->AsString;
 BrowsingPath->Text=QJson->ForcePath("BrowsingPath")->AsString;
 Env->InsertRow("BDS",QJson->ForcePath("RootDir")->AsString,true);
 Env->InsertRow("BDSINCLUDE",QJson->ForcePath("RootDir")->AsString+"\\Include",true);
 if(QJson->ForcePath("CommonDir")->AsString!="")
   Env->InsertRow("BDSCOMMONDIR",QJson->ForcePath("CommonDir")->AsString,true);
 if(QJson->ForcePath("BoostDir")->AsString!="")
   Env->InsertRow("CG_BOOST_ROOT",QJson->ForcePath("BoostDir")->AsString,true);
 if(QJson->ForcePath("Boost64Dir")->AsString!="")
   Env->InsertRow("CG_64_BOOST_ROOT",QJson->ForcePath("Boost64Dir")->AsString,true);
}
//---------------------------------------------------------------------------
