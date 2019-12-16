package com.bee.platform.business.dto;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @notes 企业管理列表 审核 产品返回数据
 **/
@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("企业管理列表 审核 产品返回数据")
public class EnterprisesWithAppDTO implements Serializable {

    private static final long serialVersionUID = 5505826824070933315L;
    /**
     * id
     */
    private Integer id;
    /**
     * 企业的名称
     */
    private String name;
    /**
     * 企业的地址
     */
    private String address;
    /**
     * 企业的管理员
     */
    private String admin;
    /**
     * 创建日期
     */
    private Date createAt;
    /**
     * 待审核企业的状态(0,未通过，1: 已通过, 2: 未审核)
     */
    private Integer type;
    /**
     * 审核状态
     */
    private String typeName;
    /**
     * 开通产品
     */
    private String apps;

}
