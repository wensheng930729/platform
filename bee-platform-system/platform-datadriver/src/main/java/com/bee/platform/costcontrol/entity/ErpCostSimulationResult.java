package com.bee.platform.costcontrol.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
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
 * 料批模拟计算结果
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
@TableName("erp_cost_simulation_result")
public class ErpCostSimulationResult extends Model<ErpCostSimulationResult> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 料批模拟id
     */
    private Integer simulationId;
    /**
     * 料批详情
     */
    private String materialDetail;
    /**
     * 理论成品品位
     */
    private BigDecimal grade;
    /**
     * 料批矿耗
     */
    private BigDecimal materialMineCost;
    /**
     * 50矿耗
     */
    private BigDecimal fiftyMineCost;
    /**
     * 碳耗
     */
    private BigDecimal carbonCost;
    /**
     * 50完全成本
     */
    private BigDecimal fiftyFullCost;
    /**
     * 完全成本
     */
    private BigDecimal fullCost;
    /**
     * 状态：1确认，0未确认
     */
    private Integer status;
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
