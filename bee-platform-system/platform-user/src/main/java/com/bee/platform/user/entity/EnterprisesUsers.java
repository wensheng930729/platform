package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @notes 企业用户关联表
 * @Author junyang.li
 * @Date 10:55 2019/3/22
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("enterprises_users")
public class EnterprisesUsers extends Model<EnterprisesUsers> {

    private static final long serialVersionUID = 1L;
    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     *	 企业id
     */
    private Integer enterpriseId;
    /**
     * 	用户id
     */
    private Integer userId;
    /**
     * 	用户电话号码
     *
     */
    private String phone;
    /**
     *	 职位
     */
    private String post;
    /**
     * 	用户账号是否被邀请
     */
    private Integer isInvite;
    /**
     * 	用户账号是否被激活
     */
    private Integer isActive;
    /**
     * 	姓名
     */
    private String nickname;
    /**
     *	姓名拼音
     */
    private String nicknamePinyin;
    /**
     *	可访问的用户列表
     */
    private String appIds;
    /**
     * 	部门ID
     */
    private Integer departmentsid;
    /**
     * 	企业用户邮箱
     */
    private String email;
    /**
     *	 固话
     */
    private String fixtel;
    /**
     * 	县级地区id
     */
    private String regionid;
    /**
     * 	详细地址
     */
    private String address;
    /**
     * 	头像
     */
//    private String head;
    /**
     * 	职位ID
     */
    private Integer zpostid;
    
    /**
     * 	角色id
     */
    private Integer roleId;
    
    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
