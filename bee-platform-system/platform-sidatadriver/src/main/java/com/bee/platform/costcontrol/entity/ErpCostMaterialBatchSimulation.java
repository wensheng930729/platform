package com.bee.platform.costcontrol.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 料批模拟
 * </p>
 *
 * @author xin.huang
 * @since 2019-06-25
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_cost_material_batch_simulation")
public class ErpCostMaterialBatchSimulation extends Model<ErpCostMaterialBatchSimulation> {

    private static final long serialVersionUID = 1L;

    /**
     * 料批模拟id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 料批名称
     */
    private String name;
    /**
     * 成本配置id
     */
    private Integer allocationId;
    /**
     * 成本配置详情(json数据)
     */
    private String allocationInfo;
    /**
     * 创建人id
     */
    private Integer creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 0未删除，1删除
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
