// ---------------------------------------------------------------------------
#include <fmx.h>
#pragma hdrstop

#include "main.h"

// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner)
	{
	}
#define DELPHI_ANON(AType,ATypeName,Code) \
  class ATypeName:public TCppInterfacedObject<AType>\
	{\
	public:\
	  void __fastcall Invoke##Code\
	};

// ---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
	{
	TQXMLNode *ANode = new TQXMLNode;
	TMemoryStream *AStream = new TMemoryStream;
	DELPHI_ANON(TQXMLRttiFilterEventA, TButtonCallback,
		(TQXMLNode * ASender, void * AObject, System::UnicodeString AName,
			System::Typinfo::PTypeInfo AType, bool &Accept, void * ATag) {
			if (AName == L"Name")
				Accept = true;
			else
				Accept = false;});
	try
		{
		IdHTTP1->Get(L"http://www.eryatang.com/Customer.xml", AStream);
		AStream->Position = 0;
		ANode->LoadFromStream(AStream);
		ShowMessage(ANode->AsXML);
		// ANode->Clear();
		// ANode->AddObject(L"Button1", Button1, new TButtonCallback(),
		// false, NULL);
		// ShowMessage(ANode->AsXML);
		}
	__finally
		{
		delete AStream;
		delete ANode;
		}
	}

// ---------------------------------------------------------------------------
void __fastcall TForm1::btnParseClick(TObject *Sender)
	{
	TQXMLNode *ANode = new TQXMLNode;
	TMemoryStream *AStream = new TMemoryStream;
	try
		{
		IdHTTP1->Get(L"http://www.eryatang.com/Customer.xml", AStream);
		AStream->Position = 0;
		ANode->LoadFromStream(AStream);
		ListView1->Items->Clear();
		TListViewItem *AItem = ListView1->Items->Add();
		AItem->Text = ANode->ItemWithAttrValue(L"customer.field", L"id",
			"firstName")->Text + L" " + ANode->ItemWithAttrValue
			(L"customer.field", L"id", "lastName")->Text;
		AItem->Detail = L"From " + ANode->ItemWithAttrValue(L"customer.field",
			L"id", "country")->Text + L" email:" + ANode->ItemWithAttrValue
			(L"customer.field", L"id", "email")->Text;
		AItem->ButtonText = L"Detail...";
		}
	__finally
		{
		delete AStream;
		delete ANode;
		}
	}

// ---------------------------------------------------------------------------
void __fastcall TForm1::EnumNodes(TQXMLNode *ANode)
	{
	TListViewItem *AChild;
	for (int i = 0; i < ANode->Count; i++)
		{
		AChild = ListView1->Items->Add();
		AChild->Text = ANode->Items[i]->Name;
		AChild->Detail = ANode->Items[i]->AsXML;
		EnumNodes(ANode->Items[i]);
		}
	}

void __fastcall TForm1::ListView1ButtonClick(const TObject *Sender,
	const TListViewItem *AItem, const TListItemSimpleControl *AObject)
	{
	ShowMessage(AItem->Detail);
	}
// ---------------------------------------------------------------------------
