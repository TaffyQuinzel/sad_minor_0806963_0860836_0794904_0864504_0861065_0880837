using System;
using System.Collections;
using System.Windows;
using System.Windows.Media;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
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
        /// <summary> 
        /// Defines the visual format for the "ordinary" classification type 
        /// </summary> 
        public dt()
        {
            this.DisplayName = "data"; //human readable version of the name 
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
        /// <summary> 
        /// Defines the visual format for the "ordinary" classification type 
        /// </summary> 
        public tpfnc()
        {
            this.DisplayName = "typefunc"; //human readable version of the name 
            this.ForegroundColor = Colors.MediumAquamarine;
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
        /// <summary> 
        /// Defines the visual format for the "ordinary" classification type 
        /// </summary> 
        public mprt()
        {
            this.DisplayName = "import"; //human readable version of the name 
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
        /// <summary> 
        /// Defines the visual format for the "ordinary" classification type 
        /// </summary> 
        public fnc()
        {
            this.DisplayName = "func"; //human readable version of the name 
            this.ForegroundColor = Colors.MediumAquamarine;
        }
    }
}
