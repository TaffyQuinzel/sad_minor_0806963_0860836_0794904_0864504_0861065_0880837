using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace McSyntax
{
    internal static class OrdinaryClassificationDefinition
    {
        /*
            The different Type declarations are defined here. 
        */
        [Export(typeof(ClassificationTypeDefinition))]
        [Name("import")]
        internal static ClassificationTypeDefinition import = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("TypeFunc")]
        internal static ClassificationTypeDefinition TypeFunc = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("Func")]
        internal static ClassificationTypeDefinition Func = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("Data")]
        internal static ClassificationTypeDefinition Data = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("String")]
        internal static ClassificationTypeDefinition String = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("Signature")]
        internal static ClassificationTypeDefinition Signature = null;
    }
}