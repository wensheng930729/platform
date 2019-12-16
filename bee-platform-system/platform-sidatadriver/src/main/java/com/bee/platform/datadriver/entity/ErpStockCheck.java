package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 库存盘点主单表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_stock_check")
public class ErpStockCheck extends Model<ErpStockCheck> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 库存盘点名称
     */
    private String stockCheckName;
    /**
     * 库存盘点日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date stockCheckTime;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 备注
     */
    private String remark;
    /**
     * 确认状态（0已保存，1已确认）
     */
    private Integer state;
    /**
     * 创建人id
     */
    private Integer creatorId;
    /**
     * 创建人企业id
     */
    private Integer creatorEnterpriseId;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 附件名称
     */
    private String fileName;
    /**
     * 附件url
     */
    private String fileUrl;
    /**
     * 修改人id
     */
    private Integer modifierId;
    /**
     * 修改时间
     */
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
