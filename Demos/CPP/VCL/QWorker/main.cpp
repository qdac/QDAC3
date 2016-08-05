// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "main.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm3 *Form3;

void __fastcall FreeDataByNew(TQWorkers* ASender, TQJobDataFreeType AFreeType,
	const void * AData)
	{
	switch (AFreeType)
		{
	case jdfFreeAsC1:
		delete (void *)AData;
		break;
	case jdfFreeAsC2:
		delete[] (void *)AData;
		break;
		}
	}

// ---------------------------------------------------------------------------
__fastcall TForm3::TForm3(TComponent* Owner) : TForm(Owner)
	{
	Workers->OnCustomFreeData = FreeDataByNew;
	}

// ---------------------------------------------------------------------------
void __fastcall TForm3::Button2Click(TObject *Sender)
	{
	Workers->Post(DoPostJobDone, NULL, True);
	}
void __fastcall TForm3::DoShowMessage(TQJob *AJob)
	{
	QStringW *ps=(QStringW *)AJob->Data;
	Application->MessageBox(ps->c_str(),L"提示",MB_OK | MB_ICONINFORMATION);
	delete ps;
	}
// ---------------------------------------------------------------------------
void __fastcall TForm3::DoPostJobDone(TQJob *AJob)
	{
	Workers->Post(DoShowMessage,
		new QStringW(L"作业投寄到执行用时 " + IntToStr((GetTimestamp() - AJob->PushTime)
				/ 10) + L" ms"), True);
	}
void __fastcall TForm3::Button5Click(TObject *Sender)
{
Workers->Post(DoMainThreadJob,NULL,True);
}
//---------------------------------------------------------------------------
void __fastcall TForm3::DoMainThreadJob(TQJob *AJob)
{
ShowMessage(L"主线程作业已经执行。");
}

void __fastcall TForm3::Button20Click(TObject *Sender)
{
//DELPHI_ANON(TQJobProcA,(PQJob AJob)
//	{
//	ShowMessage(L"OK");
//	},Anon);
//Workers->Post(Anon,NULL,true);
}
//---------------------------------------------------------------------------

void __fastcall TForm3::Button3Click(TObject *Sender)
{
ShowMessage(IntToStr(GetTimestamp()));
}
//---------------------------------------------------------------------------

