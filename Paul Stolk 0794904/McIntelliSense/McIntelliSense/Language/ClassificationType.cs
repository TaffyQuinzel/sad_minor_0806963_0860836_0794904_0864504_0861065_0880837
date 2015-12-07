using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace McSyntax
{
    internal static class OrdinaryClassificationDefinition
    {
        #region Type definition 

        /// <summary> 
        /// Defines the "ordinary" classification type. 
        /// </summary> 
        [Export(typeof(ClassificationTypeDefinition))]
        [Name("import")]
        internal static ClassificationTypeDefinition import = null;

        /// <summary> 
        /// Defines the "ordinary" classification type. 
        /// </summary> 
        [Export(typeof(ClassificationTypeDefinition))]
        [Name("TypeFunc")]
        internal static ClassificationTypeDefinition TypeFunc = null;

        /// <summary> 
        /// Defines the "ordinary" classification type. 
        /// </summary> 
        [Export(typeof(ClassificationTypeDefinition))]
        [Name("Func")]
        internal static ClassificationTypeDefinition Func = null;

        /// <summary> 
        /// Defines the "ordinary" classification type. 
        /// </summary> 
        [Export(typeof(ClassificationTypeDefinition))]
        [Name("Data")]
        internal static ClassificationTypeDefinition Data = null;

        /// <summary> 
        /// Defines the "ordinary" classification type. 
        /// </summary> 
        [Export(typeof(ClassificationTypeDefinition))]
        [Name("String")]
        internal static ClassificationTypeDefinition String = null;

        #endregion
    }
}