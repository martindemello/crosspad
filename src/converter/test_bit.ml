(* Parse an ext3 superblock.
 * $Id: ext3_superblock.ml 142 2008-07-17 15:45:56Z richard.wm.jones $
 *)

open Printf

(*let () = Bitstring.debug := true*)

let bits = Bitstring.bitstring_of_file "ext3_sb"

(* The structure is straight from /usr/include/linux/ext3_fs.h *)

let () =
  match%bitstring bits with
  | s_inodes_count [@l 32] [@littleendian],       (* Inodes count *)
    s_blocks_count [@l 32] [@littleendian],       (* Blocks count *)
    s_r_blocks_count [@l 32] [@littleendian],     (* Reserved blocks count *)
    s_free_blocks_count [@l 32] [@littleendian],  (* Free blocks count *)
    s_free_inodes_count [@l 32] [@littleendian],  (* Free inodes count *)
    s_first_data_block [@l 32] [@littleendian],   (* First Data Block *)
    s_log_block_size [@l 32] [@littleendian],     (* Block size *)
    s_log_frag_size [@l 32] [@littleendian],      (* Fragment size *)
    s_blocks_per_group [@l 32] [@littleendian],   (* # Blocks per group *)
    s_frags_per_group [@l 32] [@littleendian],    (* # Fragments per group *)
    s_inodes_per_group [@l 32] [@littleendian],   (* # Inodes per group *)
    s_mtime [@l 32] [@littleendian],              (* Mount time *)
    s_wtime [@l 32] [@littleendian],              (* Write time *)
    s_mnt_count [@l 16] [@littleendian],          (* Mount count *)
    s_max_mnt_count [@l 16] [@littleendian],      (* Maximal mount count *)
    0xef53 [@l 16] [@littleendian],               (* Magic signature *)
    s_state [@l 16] [@littleendian],              (* File system state *)
    s_errors [@l 16] [@littleendian],             (* Behaviour when detecting errors *)
    s_minor_rev_level [@l 16] [@littleendian],    (* minor revision level *)
    s_lastcheck [@l 32] [@littleendian],          (* time of last check *)
    s_checkinterval [@l 32] [@littleendian],      (* max. time between checks *)
    s_creator_os [@l 32] [@littleendian],         (* OS *)
    s_rev_level [@l 32] [@littleendian],          (* Revision level *)
    s_def_resuid [@l 16] [@littleendian],         (* Default uid for reserved blocks *)
    s_def_resgid [@l 16] [@littleendian],         (* Default gid for reserved blocks *)
    s_first_ino [@l 32] [@littleendian],          (* First non-reserved inode *)
    s_inode_size [@l 16] [@littleendian],         (* size of inode structure *)
    s_block_group_nr [@l 16] [@littleendian],     (* block group # of this superblock *)
    s_feature_compat [@l 32] [@littleendian],     (* compatible feature set *)
    s_feature_incompat [@l 32] [@littleendian],   (* incompatible feature set *)
    s_feature_ro_compat [@l 32] [@littleendian],  (* readonly-compatible feature set *)
    s_uuid [@l 128] [@string],                    (* 128-bit uuid for volume *)
    s_volume_name [@l 128] [@string],             (* volume name *)
    s_last_mounted [@l 512] [@string],            (* directory where last mounted *)
    s_algorithm_usage_bitmap [@l 32] [@littleendian], (* For compression *)
    s_prealloc_blocks [@l 8],                     (* Nr of blocks to try to preallocate*)
    s_prealloc_dir_blocks [@l 8],                 (* Nr to preallocate for dirs *)
    s_reserved_gdt_blocks [@l 16] [@littleendian], (* Per group desc for online growth *)
    s_journal_uuid [@l 128] [@string],            (* uuid of journal superblock *)
    s_journal_inum [@l 32] [@littleendian],       (* inode number of journal file *)
    s_journal_dev [@l 32] [@littleendian],        (* device number of journal file *)
    s_last_orphan [@l 32] [@littleendian],        (* start of list of inodes to delete *)
    s_hash_seed0 [@l 32] [@littleendian],         (* HTREE hash seed *)
    s_hash_seed1 [@l 32] [@littleendian],
    s_hash_seed2 [@l 32] [@littleendian],
    s_hash_seed3 [@l 32] [@littleendian],
    s_def_hash_version [@l 8],                    (* Default hash version to use *)
    s_reserved_char_pad [@l 8],
    s_reserved_word_pad [@l 16] [@littleendian],
    s_default_mount_opts [@l 32] [@littleendian],
    s_first_meta_bg [@l 32] [@littleendian],      (* First metablock block group *)
    _ [@l 6080] [@bitstring] ->                   (* Padding to the end of the block *)

      printf "ext3 superblock:\n";
      printf "  s_inodes_count = %ld\n" s_inodes_count;
      printf "  s_blocks_count = %ld\n" s_blocks_count;
      printf "  s_free_inodes_count = %ld\n" s_free_inodes_count;
      printf "  s_free_blocks_count = %ld\n" s_free_blocks_count;
      printf "  s_uuid = %S\n" s_uuid;
      printf "  s_volume_name = %S\n" s_volume_name;
      printf "  s_last_mounted = %S\n" s_last_mounted

  | _ ->
      eprintf "not an ext3 superblock!\n%!";
      exit 2
