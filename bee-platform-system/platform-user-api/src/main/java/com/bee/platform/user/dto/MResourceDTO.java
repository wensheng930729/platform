package com.bee.platform.user.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-05-13 10:24
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class MResourceDTO implements Serializable {

    private static final long serialVersionUID = -6019343182715339556L;
    /**
     * 资源id
     */
    private Integer resourceId;
    /**
     * 资源名称
     */
    private String name;
    /**
     * 父级id
     */
    private Integer parentId;
    /**
     * 资源类型
     */
    private Integer resourceType;
    /**
     * 资源级别
     */
    private Integer resourceLev;
    /**
     * 资源图标
     */
    private String icon;
    /**
     * 前端给的路径
     */
    private String path;

    private String component;
    /**
     * 是否显示子菜单0否，1是，前端初始化的时候给
     */
    private Boolean hideChildrenInMenu;
    /**
     * 资源说明
     */
    private String explain;
    /**
     * 拦截等级
     */
    private Integer intercept;
    /**
     * 是否有效0无效，1有效
     */
    private Integer status;
    /**
     * 子菜单
     */
    private List<MResourceDTO> routes;


    /**
     * @notes: 返回子菜单的list对象
     * @Author: junyang.li
     * @Date: 13:52 2019/5/13
     * @return: java.util.List<com.bee.platform.user.dto.MResourceDTO>
     */
    public List<MResourceDTO> isNull(){
        if(CollectionUtils.isEmpty(routes)){
            this.routes=new ArrayList<>();
        }
        return this.routes;
    }
}
