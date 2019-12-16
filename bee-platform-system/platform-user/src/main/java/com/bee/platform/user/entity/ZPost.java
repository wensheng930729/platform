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
 * @author hongchuan.he123
 * @since 2019-04-25
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("z_post")
public class ZPost extends Model<ZPost> {

    private static final long serialVersionUID = 1L;

    /**
     * 职位ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 职位名称
     */
    private String name;
    /**
     * 部门ID
     */
    private Integer departmentId;
    /**
     * 状态：0表示删除
     */
    private Integer status;
    /**
     * 职位描述
     */
    private String description;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
