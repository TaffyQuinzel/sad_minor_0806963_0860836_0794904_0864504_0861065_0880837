using System.Windows.Media;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace McSyntax
{
    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "data")]
    [Name("data")]
    //this should be visible to the end user 
    [UserVisible(false)]
    //set the priority to be after the default classifiers 
    [Order(Before = Priority.High)]
    internal sealed class dt : ClassificationFormatDefinition
    {
        public dt()
        {
            this.DisplayName = "data"; 
            this.ForegroundColor = Colors.Orange;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "typefunc")]
    [Name("typefunc")]
    //this should be visible to the end user 
    [UserVisible(false)]
    //set the priority to be after the default classifiers 
    [Order(Before = Priority.High)]
    internal sealed class tpfnc : ClassificationFormatDefinition
    {
        public tpfnc()
        {
            this.DisplayName = "typefunc"; 
            this.ForegroundColor = Colors.SteelBlue;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "import")]
    [Name("import")]
    //this should be visible to the end user 
    [UserVisible(false)]
    //set the priority to be after the default classifiers 
    [Order(Before = Priority.High)]
    internal sealed class mprt : ClassificationFormatDefinition
    {
        public mprt()
        {
            this.DisplayName = "import"; 
            this.ForegroundColor = Colors.Blue;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "func")]
    [Name("func")]
    //this should be visible to the end user 
    [UserVisible(false)]
    //set the priority to be after the default classifiers 
    [Order(Before = Priority.High)]
    internal sealed class fnc : ClassificationFormatDefinition
    {
        public fnc()
        {
            this.DisplayName = "func"; 
            this.ForegroundColor = Colors.SteelBlue;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "signature")]
    [Name("signature")]
    //this should be visible to the end user 
    [UserVisible(false)]
    //set the priority to be after the default classifiers 
    [Order(Before = Priority.High)]
    internal sealed class sgntr : ClassificationFormatDefinition
    {
        public sgntr()
        {
            this.DisplayName = "signature"; 
            this.ForegroundColor = Colors.Purple;
        }
    }

}
