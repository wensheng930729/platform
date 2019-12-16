package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @notes 用户实体
 * @Author junyang.li
 * @Date 10:45 2019/3/4
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("users")
public class User extends Model<User> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 唯一标识
     */
    private String uuid;
    /**
     * 是否激活
     */
    private Integer isActive;
    /**
     * im账号
     */
    private String sig;
    /**
     * im密码
     */
    private String identifier;
    /**
     * 头像
     */
    private String head;
    /**
     * 密码
     */
    private String password;
    /**
     * 手机号
     */
    private String phone;
    /**
     * 邮箱
     */
    private String email;
    /**
     * 姓名
     */
    private String nickname;
    /**
     * 姓名拼音
     */
    private String nicknamePinyin;
    /**
     * 账号
     */
    private String username;
    /**
     * 创建时间
     */
    private Date createAt;
    /**
     * 更新时间
     */
    private Date updateAt;
    /**
     * 修改人id
     */
    private Integer updateId;
    /**
     * QQ
     */
    private String qq;
    /**
     * 县级地区id
     */
    private String regionid;
    /**
     * 详细地址
     */
    private String address;
    /**
     * 固话
     */
    private String fixtel;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
