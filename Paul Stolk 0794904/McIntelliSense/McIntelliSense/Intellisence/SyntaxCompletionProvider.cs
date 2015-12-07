using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Utilities;

namespace McSyntax.Intellisence
{
    [Export(typeof(ICompletionSourceProvider))]
    [ContentType("mc")]
    [Name("token completion")]
    internal class SyntaxCompletionProvider : ICompletionSourceProvider
    {
        [Import]
        internal ITextStructureNavigatorSelectorService NavigatorService { get; set; }

        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {
            return new SyntaxCompletion(this, textBuffer);
        }
    }
    
}
