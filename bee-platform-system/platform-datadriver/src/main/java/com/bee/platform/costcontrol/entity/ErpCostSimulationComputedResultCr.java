package com.bee.platform.costcontrol.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * <p>
 * 成本模拟计算结果
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_cost_simulation_computed_result_cr")
public class ErpCostSimulationComputedResultCr extends Model<ErpCostSimulationComputedResultCr> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 成本模拟基础配置id
     */
    private Integer costSimulationId;
    /**
     * 成品品味
     */
    private BigDecimal endProductTaste;
    /**
     * 碳耗
     */
    private BigDecimal carbonConsumption;
    /**
     * 矿耗
     */
    private BigDecimal oreConsumption;
    /**
     * 50矿耗
     */
    private BigDecimal fiftyOreConsumption;
    /**
     * 矿成本
     */
    private BigDecimal miningCost;
    /**
     * 碳成本
     */
    private BigDecimal carbonCost;
    /**
     * 电成本
     */
    private BigDecimal electricityCost;
    /**
     * 辅料成本
     */
    private BigDecimal accessoriesCost;
    /**
     * 三费
     */
    private BigDecimal threeCharges;
    /**
     * 制造成本
     */
    private BigDecimal manufacturingCost;
    /**
     * 50完全成本（列表需要的取值）
     */
    private BigDecimal fiftyFullCost;
    /**
     * 完全成本
     */
    private BigDecimal fullCost;
    /**
     * 类型(1报盘 2快检 3自检)
     */
    private Integer type;
    /**
     * 状态：1启动 0禁用
     */
    private Integer status;
    /**
     * 表示逻辑删除，1-是删除，0-不删除
     */
    private Integer deleted;
    /**
     * 创建人id
     */
    private Integer creatorId;
    /**
     * 创建人姓名
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 删除时间
     */
    private Date deletedTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
