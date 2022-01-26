using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace Assignment1.Models
{
    public partial class Comments
    {
        public int CommentId { get; set; }
        public int BlogPostId { get; set; }
        public int UserId { get; set; }
        [Required]
        public string Content { get; set; }

        public virtual BlogPosts BlogPost { get; set; }
        public virtual Users User { get; set; }
    }
}
