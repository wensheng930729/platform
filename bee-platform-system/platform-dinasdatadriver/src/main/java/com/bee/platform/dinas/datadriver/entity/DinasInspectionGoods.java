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
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 验货磅单表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
public class DinasInspectionGoods extends Model<DinasInspectionGoods> {

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
     * 采购合同id
     */
    private Integer purchaseOrderId;
    /**
     * 销售合同id
     */
    private Integer saleOrderId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 数量
     */
    private BigDecimal num;
    /**
     * 附件地址
     */
    private String url;
    /**
     * 验货日期
     */
    private Date inspectionDate;
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
