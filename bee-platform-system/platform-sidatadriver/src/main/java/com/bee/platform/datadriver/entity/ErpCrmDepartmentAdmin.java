package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;

import lombok.Data;
import lombok.experimental.Accessors;

import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

/**
 * <p>
 * 部门和负责人关联表
 * </p>
 *
 * @author chenjie123123
 * @since 2019-07-03
 */
@Data
@Accessors(chain = true)
public class ErpCrmDepartmentAdmin extends Model<ErpCrmDepartmentAdmin> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 部门id
     */
    private Integer departmentId;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 企业id
     */
    private Integer enterpriseId;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    @Override
    public String toString() {
        return "ErpCrmDepartmentAdmin{" +
        ", id=" + id +
        ", departmentId=" + departmentId +
        ", userId=" + userId +
        "}";
    }
}
