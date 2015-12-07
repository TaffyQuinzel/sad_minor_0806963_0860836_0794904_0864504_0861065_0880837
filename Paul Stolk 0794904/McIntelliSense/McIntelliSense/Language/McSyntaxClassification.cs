using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace McSyntax
{

    internal static class FileAndContentTypeDefinitions
    {
        [Export(typeof(ClassificationTypeDefinition))]
        [Name("mc")]
        internal static ClassificationTypeDefinition mc = null;
    }
}
