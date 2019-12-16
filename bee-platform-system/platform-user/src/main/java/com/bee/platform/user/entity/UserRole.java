package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @notes 用户角色
 * @Author junyang.li
 * @Date 17:22 2019/3/4
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("users_roles")
public class UserRole extends Model<UserRole> {

    private static final long serialVersionUID = 1L;


    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 角色id
     */
    private Integer roleId;
    /**
     * 企业id
     */
    private Integer orgId;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
