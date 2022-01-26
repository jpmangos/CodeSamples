using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace Assignment1.Models
{
    public partial class BlogPosts
    {
        public BlogPosts()
        {
            Comments = new HashSet<Comments>();
        }

        public int BlogPostId { get; set; }
        public int UserId { get; set; }

        [Required]
        [StringLength(1000)]
        public string Title { get; set; }

        [Required]
        [StringLength(1000)]
        public string Content { get; set; }
        public DateTime Posted { get; set; }
        public virtual Users User { get; set; }
        public virtual ICollection<Comments> Comments { get; set; }
    }
}
