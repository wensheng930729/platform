package com.bee.platform.dinas.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 销售调价明细表
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
@Data
@ToString
@Accessors(chain = true)
public class DinasSaleAdjustDetail extends Model<DinasSaleAdjustDetail> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 调价主表id
     */
    private Integer adjustId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品规格
     */
    private Integer productSpecId;
    /**
     * 产品规格
     */
    private String unit;
    /**
     * 不含税单价
     */
    private BigDecimal priceBefore;
    /**
     * 含税单价
     */
    private BigDecimal taxPriceBefore;
    /**
     * 不含税单价
     */
    private BigDecimal priceAfter;
    /**
     * 含税单价
     */
    private BigDecimal taxPriceAfter;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
