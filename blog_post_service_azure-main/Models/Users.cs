using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace Assignment1.Models
{
    public partial class Users
    {
        public Users()
        {
            BlogPosts = new HashSet<BlogPosts>();
            Comments = new HashSet<Comments>();
        }

        public int UserId { get; set; }
        [Required]
        [Range (1, 2)]
        public int RoleId { get; set; }
        [Required]
        [StringLength(1000)]
        public string FirstName { get; set; }
        [Required]
        [StringLength(1000)]
        public string LastName { get; set; }
        [Required]
        [StringLength(1000)]
        public string EmailAddress { get; set; }
        [Required]
        [StringLength(1000)]
        public string Password { get; set; }
        public virtual Roles Role { get; set; }
        public virtual ICollection<BlogPosts> BlogPosts { get; set; }
        public virtual ICollection<Comments> Comments { get; set; }
    }
}
