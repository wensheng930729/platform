package com.bee.platform.dinas.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableField;
import com.baomidou.mybatisplus.enums.FieldStrategy;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 销售结算表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
public class DinasSaleSettlement extends Model<DinasSaleSettlement> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 销售合同id
     */
    private Integer saleOrderId;
    /**
     * 验货磅单id
     */
    private Integer inspectionOrderId;
    /**
     * 结算单价
     */
    @TableField(strategy = FieldStrategy.IGNORED)
    private BigDecimal settlementUnitPrice;
    /**
     * 结算总价
     */
    @TableField(strategy = FieldStrategy.IGNORED)
    private BigDecimal settlementSumPrice;
    /**
     * 结算状态（0未结算 1已结算）
     */
    private Integer status;
    /**
     * 结算日期
     */
    @TableField(strategy = FieldStrategy.IGNORED)
    private Date settlementDate;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Integer updateUser;
    /**
     * 修改时间
     */
    private Date updateTime;
    /**
     * 逻辑删除（0未删除 1已删除）
     */
    private Integer deleted;



    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
