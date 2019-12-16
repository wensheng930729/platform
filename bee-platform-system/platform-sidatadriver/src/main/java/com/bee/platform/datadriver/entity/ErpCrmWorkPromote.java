
package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 工作推进
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@TableName("erp_crm_work_promote")
public class ErpCrmWorkPromote extends Model<ErpCrmWorkPromote> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 商机信息id
     */
    private Integer commercialId;
    /**
     * 拜访人
     */
    private String visitingName;
    /**
     * 拜访日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date visitingTime;
    /**
     * 拜访说明
     */
    private String visitingExplain;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTime;
    /**
     * 修改人
     */
    private Integer updateUser;
    /**
     * 修改时间
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date updateTime;
    /**
     * 备注
     */
    private String remark;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
