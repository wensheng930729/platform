package com.bee.platform.user.entity;

import java.io.Serializable;
import java.util.Date;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * <p>
 * 
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */

@Data
@NoArgsConstructor
@ToString
@Accessors(chain=true)
@TableName("departments")
public class Departments extends Model<Departments> {

    private static final long serialVersionUID = 1L;


    /**
     * 部门id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业id
     */
    private Integer orgId;
    /**
     * 部门树id
     */
    private Integer treeId;
    /**
     *  部门名称
     */
    private String name;
    /**
     * 部门创建日期
     */
    private Date createAt;
    /**
     * 部门修改日期
     */
    private Date updateAt;

    /**
     * 部门层级
     */
    private Integer level;
    /**
     * 职能描述
     */
    private String description;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
