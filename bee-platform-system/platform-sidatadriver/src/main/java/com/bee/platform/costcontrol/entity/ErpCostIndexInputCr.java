package com.bee.platform.costcontrol.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Date;

/**
 * <p>
 * 成本指标录入
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_cost_index_input_cr")
public class ErpCostIndexInputCr extends Model<ErpCostIndexInputCr> {

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
     * 三氧化二铬
     */
    private BigDecimal chromiumTrioxide;
    /**
     * 氧化亚铁
     */
    private BigDecimal ferrousOxide;
    /**
     * 氧化镁
     */
    private BigDecimal magnesiumOxide;
    /**
     * 三氧化二铝
     */
    private BigDecimal aluminumTrioxide;
    /**
     * 铬/铁比例
     */
    private BigDecimal ferrochromeRatio;
    /**
     * 镁/铝比例
     */
    private BigDecimal magnesiumAluminiumRatio;
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
    /**
     * 类型(1报盘 2快检 3自检)
     */
    private Integer type;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
