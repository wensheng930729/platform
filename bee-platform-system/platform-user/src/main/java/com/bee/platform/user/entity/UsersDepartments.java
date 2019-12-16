package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */

@Data
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("users_departments")
public class UsersDepartments extends Model<UsersDepartments> {

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
     * 部门id
     */
    private Integer departmentId;
    /**
     * 职位id
     */
    private Integer postId;
    /**
     * 职位
     */
    private String post;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
